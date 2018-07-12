{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM,BgpFSMconfig(..)) where
import Network.Socket
import System.IO.Error(catchIOError)
import qualified Data.ByteString as B
import Data.Binary(encode,decode)
import System.Timeout(timeout)
import Control.Concurrent
import Control.Exception
import Control.Monad(when)
import Data.Maybe(fromJust)
import Common
import BGPparse
import GetBGPMsg
import RFC4271
import Open
import Capabilities
import Collision

data FSMException = FSMException String
    deriving Show

instance Exception FSMException

data BgpFSMconfig = BgpFSMconfig {local :: BGPMessage,
                                  remote :: BGPMessage,
                                  sock :: Socket,
                                  collisionDetector :: CollisionDetector,
                                  peerName :: SockAddr,
                                  delayOpenTimer :: Int,
                                  exitMVar :: MVar (ThreadId,String)
                                  }
bgpFSM :: BgpFSMconfig -> IO ()
bgpFSM BgpFSMconfig{..} = do threadId <- myThreadId
                             putStrLn $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName
                             catch
                                 (stateConnected osm)
                                 (\(FSMException s) -> do
                                     deregister cd
                                     putMVar exitMVar (threadId,s)
                                     putStrLn $ "Thread " ++ show threadId ++ " exiting"
                                 ) where
    exit s = throw $ FSMException s
    initialHoldTimer = 120
    cd = collisionDetector
    osm = makeOpenStateMachine local remote
    snd msg = catchIOError ( sndBgpMessage sock $ encode msg ) (\e -> exit (show (e :: IOError)))
    get :: Int -> IO BGPMessage
    -- get t = catchIOError (get' t) (\e -> BGPError (show (e :: IOError))
    get t = catchIOError (get' t) (\e -> do putStrLn $ "IOError in get: " ++ show (e :: IOError)
                                            return BGPEndOfStream
                                  )
    get' t = let t' = t * 10000000 in
             do mMsg <- timeout t' (getBgpMessage sock)
                maybe
                    (return BGPTimeout)
                    (\msg -> do
                        let bgpMsg = decode msg :: BGPMessage
                        return bgpMsg)
                    mMsg

    stateConnected osm = do msg <- get delayOpenTimer
                            case msg of 
                                BGPTimeout -> do
                                    putStrLn "stateConnected - event: delay open expiry"
                                    snd (localOffer osm)
                                    putStrLn "stateConnected -> stateOpenSent"
                                    stateOpenSent osm
                                open@BGPOpen{} -> do
                                    let osm' = updateOpenStateMachine osm open
                                    putStrLn "stateConnected - event: rcv open"
                                    print open
                                    collisionCheck cd (bgpID $ localOffer osm) (bgpID $ fromJust $ remoteOffer osm)
                                    let resp =  getResponse osm'
                                    if isKeepalive resp then do 
                                        putStrLn "stateConnected -> stateOpenConfirm"
                                        snd (localOffer osm')
                                        stateOpenConfirm osm'
                                        snd resp
                                    else do
                                        snd resp
                                        exit "stateConnected - event: open rejected error"
                                notify@BGPNotify{} -> do
                                   print notify
                                   exit "stateConnected -> exit rcv notify"
                                _ -> do
                                    snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                                    exit "stateConnected - FSM error"

    stateOpenSent osm = do msg <- get initialHoldTimer
                           case msg of 
                             BGPTimeout -> do
                                 snd $ BGPNotify NotificationHoldTimerExpired 0 []
                                 exit "stateOpenSent - error initial Hold Timer expiry"
                             open@BGPOpen{} -> do
                                 let osm' = updateOpenStateMachine osm open
                                 putStrLn "stateOpenSent - rcv open"
                                 print open
                                 collisionCheck cd (bgpID $ localOffer osm) (bgpID $ fromJust $ remoteOffer osm)
                                 let resp =  getResponse osm'
                                 snd resp
                                 if isKeepalive resp then do 
                                     putStrLn "stateOpenSent -> stateOpenConfirm"
                                     stateOpenConfirm osm'
                                 else exit "stateOpenSent - open rejected error"
                             notify@BGPNotify{} -> do
                                print notify
                                exit "stateOpenSent - rcv notify"
                             _ -> do
                                 snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                                 exit "stateOpenSent - FSM error"

    stateOpenConfirm osm = do msg <- get (getNegotiatedHoldTime osm)
                              case msg of 
                                  BGPTimeout -> do
                                      snd $ BGPNotify NotificationHoldTimerExpired 0 []
                                      exit "stateOpenConfirm - error initial Hold Timer expiry"
                                  BGPKeepalive -> do
                                      putStrLn "stateOpenConfirm - rcv keepalive"
                                      toEstablished osm
                                  notify@BGPNotify{} -> do
                                      print notify
                                      exit "stateOpenConfirm - rcv notify"
                                  _ -> do
                                      snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                                      exit "stateOpenConfirm - FSM error"

    keepAliveLoop timer = do
        threadDelay $ 1000000 * timer
        snd BGPKeepalive
        keepAliveLoop timer

    toEstablished osm = do
        putStrLn "transition -> established"
        putStrLn $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        forkIO $ keepAliveLoop (getKeepAliveTimer osm)
        let remoteBGPid = bgpID $ fromJust $ remoteOffer osm in
            registerEstablished cd remoteBGPid peerName
        established osm

    established osm = do
        msg <- get (getNegotiatedHoldTime osm)
        case msg of 
            BGPKeepalive -> do
                putStrLn "established - rcv keepalive"
                established osm
            update@BGPUpdate{} -> do
                putStrLn "established - rcv update"
                print update
                established osm
            notify@BGPNotify{} -> do
                print notify
                exit "established - rcv notify"
            BGPEndOfStream -> exit "established: BGPEndOfStream"
            BGPTimeout -> do
                snd $ BGPNotify NotificationHoldTimerExpired 0 []
                exit "established - HoldTimerExpired error"
            _ -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                exit "established - FSM error"

    -- collisionCheck
    -- manage cases where there is an established connection (always reject)
    -- and where another connection is in openSent state (use tiebreaker)
    -- and of couse where there is no other connection for this BGPID
    collisionCheck :: CollisionDetector -> IPv4 -> IPv4 -> IO ()
    collisionCheck c localBgpid remoteBgpid = do
        putStrLn "collisionCheck:1"
        rc <- raceCheck c remoteBgpid peerName
        putStrLn "collisionCheck:2"
        maybe
            -- (return ())
            (putStrLn "collisionCheck:3")
            (\session -> do
                putStrLn "collisionCheck:4"
                when (sessionEstablished session || remoteBgpid > localBgpid) $
                    do snd $ BGPNotify NotificationCease 0 []
                       putStrLn $ "collision detected with " ++ show session
                       if sessionEstablished session then
                           exit "collisionCheck - event: collision with established session - open rejected error"
                       else
                           exit "collisionCheck - event: collision with tie-break - open rejected error"
                )
            rc
