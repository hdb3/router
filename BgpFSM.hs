{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM,BgpFSMconfig(..)) where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(Handle,stdout,hFlush)
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
import BGPData
import GetBGPMsg
import RFC4271
import Open
import Capabilities
import Collision
import Update
import PathAttributes
import Prefixes
import Rib
import Route
import PrefixTableUtils

type F = (BufferedSocket,OpenStateMachine) -> IO (State,BufferedSocket,OpenStateMachine)
type FSMExit = ( ThreadId, SockAddr, Either String String )

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

newtype FSMException = FSMException String
    deriving Show

instance Exception FSMException

data BgpFSMconfig = BgpFSMconfig {
                                  sock :: Socket,
                                  collisionDetector :: CollisionDetector,
                                  peerName :: SockAddr,
                                  delayOpenTimer :: Int,
                                  exitMVar :: MVar FSMExit
                                  , logFile :: Maybe Handle
                                  , peerData :: PeerData
                                  , rib :: Rib.Rib
                                  }
bgpFSM :: BgpFSMconfig -> IO ()

bgpFSM BgpFSMconfig{..} = do threadId <- myThreadId
                             putStrLn $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName
                             fsmExitStatus <-
                                 catch
                                 (fsm (StateConnected,bsock0,osm) )
                                 (\(FSMException s) ->
                                     return $ Left s
                                 )
                             close sock
                             logFlush bsock0
                             deregister cd
                             putMVar exitMVar (threadId , peerName, fsmExitStatus )
                             either
                                 (\s -> putStrLn $ "BGPfsm exception exit" ++ s)
                                 (\s -> putStrLn $ "BGPfsm normal exit" ++ s)
                                 fsmExitStatus
                             where
    bsock0 = newBufferedSocket sock logFile
    exit s = throw $ FSMException s
    initialHoldTimer = 120
    cd = collisionDetector
    osm = makeOpenStateMachine myOpen remote
    myBGPID = myBGPid $ globalData peerData
    localAS = fromIntegral (myAS (globalData peerData)) -- conflict between 16 and 32 bit ASN types!!
    remoteAS = fromIntegral $ peerAS peerData            -- conflict between 16 and 32 bit ASN types!!
    myOpen = BGPOpen localAS (propHoldTime peerData) myBGPID (offerCapabilies peerData)
    remote = BGPOpen remoteAS (reqHoldTime peerData) (peerBGPid peerData) (requireCapabilies peerData)
    snd msg | 4079 > L.length (encode msg) = catchIOError ( sndBgpMessage bsock0 (encode msg)) (\e -> exit (show (e :: IOError)))

    get :: BufferedSocket -> Int -> IO (BufferedSocket,BGPMessage)
    get b t = do (next,bytes) <- getMsg b t
                 return (next, decodeBGPByteString bytes )

    fsm :: (State,BufferedSocket,OpenStateMachine) -> IO (Either String String)
    fsm (s,b,o) | s == Idle = do
                                logFlush bsock0
                                let s = "FSM exiting" ++ rcvStatus (result b)
                                return $ Right s
                | otherwise = do
        (s',b',o') <- f s (b,o)
        fsm (s',b',o') where
            f StateConnected = stateConnected
            f StateOpenSent = stateOpenSent
            f StateOpenConfirm = stateOpenConfirm
            f ToEstablished = toEstablished
            f Established = established

    idle s = do putStrLn $ "IDLE - reason: " ++ s
                return (Idle,newBufferedSocket undefined undefined,undefined)

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
        running <- catch
            (do snd BGPKeepalive
                return True)
            (\(FSMException s) -> do
                -- putStrLn "keepAliveLoop exiting on snd failure"
                -- this is perfectly normal event when the fsm closes down as it doesn't stop the keepAliveLoop explicitly
                -- TODO - integrate keepAliveLoop with update dissemination....
                return False
            )
        when running
            ( keepAliveLoop timer )

    toEstablished :: F
    toEstablished (bsock,osm) = do
        putStrLn "transition -> established"
        putStrLn $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        forkIO $ keepAliveLoop (getKeepAliveTimer osm)
        let remoteBGPid = bgpID $ fromJust $ remoteOffer osm in
            registerEstablished cd remoteBGPid peerName
        addPeer rib peerData -- shoudl update it with the received parameters!!!!
        return (Established,bsock,osm)

    established :: F
    established (bsock,osm) = do
        (bsock',msg) <- get bsock (getNegotiatedHoldTime osm)
        case msg of 
            BGPKeepalive -> do
                logFlush bsock0
                putStr "established - rcv keepalive"
                prefixTable <- Rib.getRib rib
                if length prefixTable < 10 then do
                    -- putStrLn $ "Prefix Table (" ++ show (length prefixTable) ++ ")"
                    -- putStrLn $ showPrefixTableByRoute prefixTable
                    -- putStrLn $ showPrefixTable prefixTable
                    putStr $ "\rprefixTable contains " ++ show ( length prefixTable ) ++ " prefixes"
                else
                    putStr $ "\rprefixTable contains " ++ show ( length prefixTable ) ++ " prefixes"
                updates <- pullAllUpdates peerData rib 
                if null updates then return ()
                else if 11 > length updates then do
                    putStr "\nReady to send routes....:"
                    -- print $ map fst updates
                else do
                    putStr "\nReady to send routes....:"
                    -- print $ map fst (take 10 updates)
                    putStrLn $ "and " ++ show (length updates - 10) ++ " more"
                routes <- lookupRoutes rib peerData updates
                mapM_ snd routes
                hFlush stdout
                return (Established,bsock',osm)
            update@BGPUpdate{} ->
                maybe
                    ( do
                         snd $ BGPNotify NotificationUPDATEMessageError 0 L.empty
                         idle "established - Update parse error"
                    )
                    (\parsedUpdate -> do
                      let routeData = Rib.makeRouteData peerData parsedUpdate
                      -- let routeData = Rib.makeRouteData peerData (puPathAttributes parsedUpdate) (hash parsedUpdate)
                      Rib.ribUpdater rib routeData parsedUpdate
                      return (Established,bsock',osm)
                    )
                    ( processUpdate update )

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
