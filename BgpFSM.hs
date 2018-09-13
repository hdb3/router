{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM,BgpFSMconfig(..)) where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(hFlush,Handle)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Control.Concurrent
import Control.Exception
import Control.Monad(when)
import Data.Maybe(fromJust,isJust)
import Data.Either(either)
import qualified Data.Map.Strict as Data.Map

import Common
import BGPparse
import BGPData
import GetBGPMsg
import RFC4271
import Open
import Collision
import Update
import Rib
import Route
import PrefixTableUtils
import Global

-- TODO - modify the putStrLn's to at least report the connected peer. but..
-- better: implement a logger

type F = (BufferedSocket,OpenStateMachine) -> IO (State,BufferedSocket,OpenStateMachine)
-- type FSMExit = ( ThreadId, SockAddr, Either String String )

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

newtype FSMException = FSMException String
    deriving Show

instance Exception FSMException

-- data BgpFSMconfig = BgpFSMconfig {
                                  -- sock :: Socket,
                                  -- collisionDetector :: CollisionDetector,
                                  -- peerName :: SockAddr,
                                  -- delayOpenTimer :: Int,
                                  -- exitMVar :: MVar FSMExit
                                  -- , logFile :: Maybe Handle
                                  -- , peerData :: PeerData
                                  -- , rib :: Rib.Rib
                                  -- }
bgpFSM :: Global -> ( Socket , SockAddr) -> IO ()

bgpFSM Global{..} ( sock , peerName ) =
                          do threadId <- myThreadId
                             putStrLn $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName
                             logFile <- getLogFile
                             -- let peerData = fromJust $ Data.Map.lookup (getIPv4 peerName) peerMap
                             -- TODO this where to handle an unconfigured peer....
                             fsmExitStatus <-
                                 catch
                                 (fsm (StateConnected,bsock0,osm) )
                                 (\(FSMException s) -> do
                                     -- TODO make all finalisation stuff in one place
                                     -- after catching any/all exceptions.....
                                     -- and bracketed by corresponding initilisation....
                                     delPeer rib peerData
                                     return $ Left s
                                 )
                             close sock
                             -- fmap hFlush logFile
                             deregister cd
                             putMVar exitMVar (threadId , peerName, fsmExitStatus )
                             either
                                 (\s -> putStrLn $ "BGPfsm exception exit" ++ s)
                                 (\s -> putStrLn $ "BGPfsm normal exit" ++ s)
                                 fsmExitStatus
                             where
    getIPv4 (SockAddrInet portNumber hostAddress) = fromHostAddress hostAddress
    peerData = fromJust $ Data.Map.lookup (getIPv4 peerName) peerMap
    bsock0 = newBufferedSocket sock Nothing
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
                                -- hFlush logFile
                                let s = "FSM exiting" ++ rcvStatus (result b)
                                delPeer rib peerData
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
            update@BGPUpdate{} -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateConnected - recvd Update - FSM error"
            z -> do
                idle $ "stateConnected - network exception - " ++ show z

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

    toEstablished :: F
    toEstablished (bsock,osm) = do
        putStrLn "transition -> established"
        putStrLn $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        let remoteBGPid = bgpID $ fromJust $ remoteOffer osm in
            registerEstablished cd remoteBGPid peerName
        addPeer rib peerData
        forkIO $ keepAliveLoop rib peerData  (getKeepAliveTimer osm)
        return (Established,bsock,osm)

    established :: F
    established (bsock,osm) = do
        (bsock',msg) <- get bsock (getNegotiatedHoldTime osm)
        case msg of 
            BGPKeepalive -> do
                -- hFlush logFile
                putStrLn "established - rcv keepalive"
                return (Established,bsock',osm)
            update@BGPUpdate{} ->
                maybe
                    ( do
                         snd $ BGPNotify NotificationUPDATEMessageError 0 L.empty
                         idle "established - Update parse error"
                    )
                    (\parsedUpdate -> do
                      let routeData = Rib.makeRouteData peerData parsedUpdate
                      Rib.ribUpdater rib peerData parsedUpdate
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

    keepAliveLoop rib peer timer = do
        running <- catch
            ( do sendQueuedUpdates rib peer timer
                 return True
            )
            (\(FSMException s) -> do
                -- this is perfectly normal event when the fsm closes down as it doesn't stop the keepAliveLoop explicitly
                return False
            )
        when running
            ( keepAliveLoop rib peer timer )

-- TODO merge sendQueuedUpdates in keepAliveLoop?
    sendQueuedUpdates rib peer timeout = do
        updates <- pullAllUpdates (1000000 * timeout) peer rib
        if null updates then
            snd BGPKeepalive
        else do routes <- lookupRoutes rib peer updates
                putStr $ "Ready to send routes to " ++ show (peerIPv4 peer)
                if 11 > length updates then do
                    print $ map fst updates
                else do
                    print $ map fst (take 10 updates)
                    putStrLn $ "and " ++ show (length updates - 10) ++ " more"
                mapM_ snd routes

getLogFile = do
    t <- utcSecs
    -- TODO make unique names because multiple peers may start at the same time....
    -- handle <- openBinaryFile ("trace/" ++ show t ++ ".bgp") WriteMode
    return Nothing
    -- return $ Just handle
