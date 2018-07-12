{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM,BgpFSMconfig(..)) where
import Network.Socket
import qualified Data.ByteString as B
import Data.Binary(encode,decode)
import System.Timeout(timeout)
import Control.Concurrent
import Control.Monad(when)
import Data.Maybe(fromJust)
import Common
import BGPparse
import GetBGPMsg
import RFC4271
import Open
import Capabilities
import Collision

holdTimer = 15
initialHoldTimer = 120
data BgpFSMconfig = BgpFSMconfig {local :: BGPMessage,
                                  remote :: BGPMessage,
                                  sock :: Socket,
                                  collisionDetector :: CollisionDetector,
                                  peerName :: SockAddr,
                                  delayOpenTimer :: Int,
                                  exitMVar :: MVar (ThreadId,String)
                                  }
bgpFSM :: BgpFSMconfig -> IO ()
bgpFSM BgpFSMconfig{..} = stateConnected osm where
    cd = collisionDetector
    osm = makeOpenStateMachine local remote
    snd msg = sndBgpMessage sock $ encode msg
    get' :: Int -> IO BGPMessage
    get' t = let t' = t * 10000000 in
             do mMsg <- timeout t' (getBgpMessage sock)
                maybe
                    (return BGPTimeout)
                    (\msg -> do
                        let bgpMsg = decode msg :: BGPMessage
                        return bgpMsg)
                    mMsg

    get = do msg <- getBgpMessage sock
             let bgpMsg = decode msg :: BGPMessage
             print bgpMsg
             return bgpMsg

    stateConnected osm = do msg <- get' delayOpenTimer
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
                                    let remoteBGPid = bgpID $ fromJust $ remoteOffer osm
                                        localBGPid = bgpID $ localOffer osm in
                                        collisionCheck cd localBGPid remoteBGPid
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

    stateOpenSent osm = do msg <- get' initialHoldTimer
                           case msg of 
                             BGPTimeout -> do
                                 snd $ BGPNotify NotificationHoldTimerExpired 0 []
                                 exit "stateOpenSent - error initial Hold Timer expiry"
                             open@BGPOpen{} -> do
                                 let osm' = updateOpenStateMachine osm open
                                 putStrLn "stateOpenSent - rcv open"
                                 print open
                                 let remoteBGPid = bgpID $ fromJust $ remoteOffer osm
                                     localBGPid = bgpID $ localOffer osm in
                                     collisionCheck cd localBGPid remoteBGPid
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

    stateOpenConfirm osm = do msg <- get' holdTimer
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

    exit s = do putStrLn s
                deregister cd
                threadId <- myThreadId
                putMVar exitMVar (threadId,s)
                threadDelay $ 1000000 * 10
                -- fail s

    keepAliveLoop timer = do
        threadDelay $ 1000000 * timer
        snd BGPKeepalive
        keepAliveLoop timer

    toEstablished osm = do
        putStrLn "transition -> established"
        forkIO $ keepAliveLoop (getKeepAliveTimer osm)
        let remoteBGPid = bgpID $ fromJust $ remoteOffer osm in
            registerEstablished cd remoteBGPid peerName
        established osm

    established osm = do
        msg <- get' holdTimer
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
            BGPTimeout -> do
                snd $ BGPNotify NotificationHoldTimerExpired 0 []
                exit "established - FSM error"
            _ -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                exit "established - FSM error"




    -- collisionCheck
    -- manage cases where there is an established connection (always reject)
    -- and where another connection is in openSent state (use tiebreaker)
    -- and of couse where there is no other connection for this BGPID
    collisionCheck :: CollisionDetector -> IPv4 -> IPv4 -> IO ()
    collisionCheck c localBgpid remoteBgpid = do
        rc <- raceCheck c remoteBgpid peerName
        maybe
            (return ())
            (\session ->
                when (sessionEstablished session || remoteBgpid > localBgpid) $
                    do snd $ BGPNotify NotificationCease 0 []
                       if sessionEstablished session then
                           exit "collisionCheck - event: collision with established session - open rejected error"
                       else
                           exit "collisionCheck - event: collision with tie-break - open rejected error"
                )
            rc
            -- Nothing
