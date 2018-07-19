{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM,BgpFSMconfig(..)) where
import Network.Socket
import System.IO.Error(catchIOError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode,decode,decodeOrFail)
import Control.Concurrent
import Control.Exception
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust)
import Data.Either(either)
import Data.Int(Int64)

import Common
import BGPparse
import GetBGPMsg
import RFC4271
import Open
import Capabilities
import Collision
import PathAttributes
import Prefixes

type F = (BufferedSocket,OpenStateMachine) -> IO (State,BufferedSocket,OpenStateMachine)

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

newtype FSMException = FSMException String
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
                                 (fsm (StateConnected,bsock0,osm) )
                                 (\(FSMException s) -> do
                                     deregister cd
                                     putMVar exitMVar (threadId,s)
                                     putStrLn $ "Thread " ++ show threadId ++ " exiting"
                                 ) where
    bsock0 = newBufferedSocket sock
    exit s = throw $ FSMException s
    initialHoldTimer = 120
    cd = collisionDetector
    osm = makeOpenStateMachine local remote
    myOpen = local
    myBGPID = bgpID myOpen
    snd msg = catchIOError ( sndBgpMessage bsock0 (encode msg)) (\e -> exit (show (e :: IOError)))

    get :: BufferedSocket -> Int -> IO (BufferedSocket,BGPMessage)
    get b t = do (next,bytes) <- getMsg b t
                 return (next, decodeBGPByteString bytes )

    fsm :: (State,BufferedSocket,OpenStateMachine) -> IO()
    fsm (s,b,o) | s == Idle = putStrLn $ "FSM exiting" ++  show ( rcvStatus $ result b)
                | otherwise = do
        -- putStrLn $ "FSM executing " ++ show s
        (s',b',o') <- (f s) (b,o)
        fsm (s',b',o') where
            f StateConnected = stateConnected
            f StateOpenSent = stateOpenSent
            f StateOpenConfirm = stateOpenConfirm
            f ToEstablished = toEstablished
            f Established = established

    idle s = do putStrLn $ "IDLE - reason: " ++ s
                return (Idle,newBufferedSocket undefined,undefined)
    stateConnected :: F
    stateConnected (bsock,osm) = do
        (bsock',msg) <- get bsock delayOpenTimer
        case msg of 
            BGPTimeout -> do
                putStrLn "stateConnected - event: delay open expiry"
                snd myOpen
                putStrLn "stateConnected -> stateOpenSent"
                return (StateOpenSent,bsock',osm)
            open@BGPOpen{} -> do
                let osm' = updateOpenStateMachine osm open
                    resp = getResponse osm'
                putStrLn "stateConnected - event: rcv open"
                print open
                collision <- collisionCheck cd myBGPID (bgpID open)
                if isJust collision then do
                    snd $ BGPNotify NotificationCease 0 L.empty
                    idle (fromJust collision)
                else if isKeepalive resp then do 
                    putStrLn "stateConnected -> stateOpenConfirm"
                    snd myOpen
                    snd resp
                    return (StateOpenConfirm,bsock',osm')
                else do
                    snd resp
                    idle "stateConnected - event: open rejected error"
            notify@BGPNotify{} -> do
               print notify
               idle "stateConnected -> exit rcv notify"
            _ -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateConnected - FSM error"

    stateOpenSent :: F
    stateOpenSent (bsock,osm) = do
        (bsock',msg) <- get bsock initialHoldTimer
        case msg of 
          BGPTimeout -> do
              snd $ BGPNotify NotificationHoldTimerExpired 0 L.empty
              idle "stateOpenSent - error initial Hold Timer expiry"
          open@BGPOpen{} -> do
              let osm' = updateOpenStateMachine osm open
                  resp =  getResponse osm'
              putStrLn "stateOpenSent - rcv open"
              print open
              collision <- collisionCheck cd myBGPID (bgpID open)
              if isJust collision then do
                  snd $ BGPNotify NotificationCease 0 L.empty
                  idle (fromJust collision)
              else if isKeepalive resp then do 
                  snd resp
                  putStrLn "stateOpenSent -> stateOpenConfirm"
                  return (StateOpenConfirm,bsock',osm')
              else idle "stateOpenSent - open rejected error"
          notify@BGPNotify{} -> do
             print notify
             idle "stateOpenSent - rcv notify"
          _ -> do
              snd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
              idle "stateOpenSent - FSM error"

    stateOpenConfirm :: F
    stateOpenConfirm (bsock,osm) = do
        (bsock',msg) <- get bsock (getNegotiatedHoldTime osm)
        case msg of 
            BGPTimeout -> do
                snd $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "stateOpenConfirm - error initial Hold Timer expiry"
            BGPKeepalive -> do
                putStrLn "stateOpenConfirm - rcv keepalive"
                return (ToEstablished,bsock',osm)
            notify@BGPNotify{} -> do
                print notify
                idle "stateOpenConfirm - rcv notify"
            _ -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateOpenConfirm - FSM error"

    keepAliveLoop timer = do
        threadDelay $ 1000000 * timer
        snd BGPKeepalive
        keepAliveLoop timer

    toEstablished :: F
    toEstablished (bsock,osm) = do
        putStrLn "transition -> established"
        putStrLn $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        forkIO $ keepAliveLoop (getKeepAliveTimer osm)
        let remoteBGPid = bgpID $ fromJust $ remoteOffer osm in
            registerEstablished cd remoteBGPid peerName
        return (Established,bsock,osm)

    established :: F
    established (bsock,osm) = do
        (bsock',msg) <- get bsock (getNegotiatedHoldTime osm)
        case msg of 
            BGPKeepalive -> do
                putStrLn "established - rcv keepalive"
                return (Established,bsock',osm)
            update@BGPUpdate{..} -> do
 
{-
 -- diagnostic code
                putStrLn "established - rcv update"
                let attributesE = decodeOrFail pathAttributes :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [PathAttribute])
                either
                    (\(_,offset,msg) -> do
                        snd update
                        putStrLn $ toHex' pathAttributes
                        -- putStrLn $ prettyHex' pathAttributes
                        exit (msg ++ " at position " ++ show offset)
                    )
                    (\(_,_,attributes) -> do
                        putStrLn "attributes"
                        print attributes
                    )
                    attributesE
 -- end diagnostic code
-}

-- verbose code....
{-
                putStrLn "attributes"
                let attributes = decode pathAttributes :: [PathAttribute]
                print attributes
                putStrLn "nrli"
                let prefixes = decode nlri :: [Prefix]
                print prefixes
                putStrLn "withdrawn"
                let withdrawn = decode withdrawnRoutes :: [Prefix]
                print withdrawn
                putStrLn "---------------------"
                -- sendToRIB update
-}
                let parsedAttributes = decode pathAttributes :: [PathAttribute]
                    prefixes = decode nlri :: [Prefix]
                    withdrawn = decode withdrawnRoutes :: [Prefix]
                    valid = (null prefixes && null parsedAttributes) || checkForRequiredPathAttributes parsedAttributes
                    endOfRIB = null prefixes && null withdrawn
                if endOfRIB then
                    putStrLn "End-of-RIB"
                else if not valid then do
                    putStrLn "***Invalid Update!!!"
                    print parsedAttributes
                    print prefixes
                    print withdrawn
                else putChar '.'
                return (Established,bsock',osm)
            notify@BGPNotify{} -> do
                print notify
                idle "established - rcv notify"
            BGPEndOfStream -> idle "established: BGPEndOfStream"
            BGPTimeout -> do
                snd $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "established - HoldTimerExpired error"
            _ -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "established - FSM error"

    -- collisionCheck
    -- manage cases where there is an established connection (always reject)
    -- and where another connection is in openSent state (use tiebreaker)
    -- and of couse where there is no other connection for this BGPID
    collisionCheck :: CollisionDetector -> IPv4 -> IPv4 -> IO (Maybe String)
    collisionCheck c self peer = do
        rc <- raceCheck c peer peerName
        maybe
            (return Nothing)
            (\session -> return $
                       if sessionEstablished session then
                           Just $ "collisionCheck - event: collision with established session - open rejected error for peer " ++ show session
                       -- TODO - check this logic
                       -- does it consider whether we initiated the connection or not?
                       -- this requires to look at the port numbers
                       else if peer < self then
                           Just $ "collisionCheck - event: collision with tie-break - open rejected error for peer "  ++ show session
                       else
                           Nothing
                )
            rc
