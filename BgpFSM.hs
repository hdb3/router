{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM) where
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
import Global

-- TODO - modify the putStrLn's to at least report the connected peer. but..
-- better: implement a logger

type F = (BufferedSocket,OpenStateMachine) -> IO (State,BufferedSocket,OpenStateMachine)

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

newtype FSMException = FSMException String
    deriving Show

instance Exception FSMException

bgpFSM :: Global -> ( Socket , SockAddr) -> IO ()
bgpFSM global@Global{..} ( sock , peerName ) =
                          do threadId <- myThreadId
                             putStrLn $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName
                             logFile <- getLogFile
                             bsock0 <- newBufferedSocket sock Nothing
                             let 
                                 initialisePeer :: Global -> Socket -> IO (Maybe PeerData)
                                 initialisePeer Global{..} sock = do
                                     SockAddrInet localPort localIP <- getSocketName sock
                                     SockAddrInet remotePort remoteIP <- getPeerName sock
                                     let 
                                         updatePeerData pd = pd { globalData = gd ,  peerIPv4 = fromHostAddress remoteIP ,  localIPv4 = fromHostAddress localIP }
                                     return $ maybe 
                                                   ( fmap updatePeerData defaultPeerData )
                                                   Just
                                                   ( Data.Map.lookup (fromHostAddress remoteIP) peerMap )

                             maybePeer <- initialisePeer global sock
                             fsmExitStatus <-
                                 maybe
                                     (do bgpSnd bsock0 $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionRejected L.empty
                                         return  $ Left "connection rejected for unconfigured peer" )
                                     (\peerData -> do
                                         catch
                                             (runFSM global bsock0 peerData )
                                             (\(FSMException s) -> do
                                                 -- TODO make all finalisation stuff in one place
                                                 -- after catching any/all exceptions.....
                                                 -- and bracketed by corresponding initilisation....
                                                 delPeer rib peerData
                                                 return $ Left s)
                                     )
                                     maybePeer
                             close sock
                             -- fmap hFlush logFile
                             deregister collisionDetector
                             putMVar exitMVar (threadId , peerName, fsmExitStatus )
                             either
                                 (\s -> putStrLn $ "BGPfsm exception exit" ++ s)
                                 (\s -> putStrLn $ "BGPfsm normal exit" ++ s)
                                 fsmExitStatus


bgpSnd :: BufferedSocket -> BGPMessage -> IO()
bgpSnd bsock msg | 4079 > L.length (encode msg) = catchIOError ( sndBgpMessage bsock (encode msg)) (\e -> throw $ FSMException (show (e :: IOError)))

get :: BufferedSocket -> Int -> IO (BufferedSocket,BGPMessage)
get b t = do (next,bytes) <- getMsg b t
             return (next, decodeBGPByteString bytes )

runFSM :: Global -> BufferedSocket -> PeerData -> IO (Either String String)
runFSM Global{..} bsock0 peerData  = do
    let initialiseOSM :: PeerData -> OpenStateMachine
        initialiseOSM PeerData{..} = makeOpenStateMachine ( BGPOpen { myAutonomousSystem = toAS2 $ myAS globalData , holdTime = propHoldTime , bgpID = myBGPid globalData , caps = offerCapabilies} )
                                                          ( BGPOpen { myAutonomousSystem = toAS2 $ peerAS , holdTime = reqHoldTime , bgpID = peerBGPid , caps = requireCapabilies} )

    fsm (StateConnected,bsock0,initialiseOSM peerData)

    where

    fsm :: (State,BufferedSocket,OpenStateMachine) -> IO (Either String String)
    fsm (s,b,o) | s == Idle = do
                                -- hFlush logFile
                                -- delPeer rib peerData
                                -- it appears that the line above is redndant...
                                return $ Right "FSM normal exit"
                | otherwise = do
        (s',b',o') <- f s (b,o)
        fsm (s',b',o') where
            f StateConnected = stateConnected
            f StateOpenSent = stateOpenSent
            f StateOpenConfirm = stateOpenConfirm
            f ToEstablished = toEstablished
            f Established = established

    idle s = do putStrLn $ "IDLE - reason: " ++ s
                return (Idle, undefined, undefined)

    stateConnected :: F
    stateConnected (bsock,osm) = do
        (bsock',msg) <- get bsock delayOpenTimer
        case msg of 
            BGPTimeout -> do
                putStrLn "stateConnected - event: delay open expiry"
                bgpSnd bsock (localOffer osm)
                putStrLn "stateConnected -> stateOpenSent"
                return (StateOpenSent,bsock',osm)
            open@BGPOpen{} -> do
                let osm' = updateOpenStateMachine osm open
                    resp = getResponse osm'
                putStrLn "stateConnected - event: rcv open"
                collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
                if isJust collision then do
                    bgpSnd bsock $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionCollisionResolution L.empty
                    idle (fromJust collision)
                else if isKeepalive resp then do 
                    putStrLn "stateConnected -> stateOpenConfirm"
                    bgpSnd bsock (localOffer osm)
                    bgpSnd bsock resp
                    return (StateOpenConfirm,bsock',osm')
                else do
                    bgpSnd bsock resp
                    idle "stateConnected - event: open rejected error"
            notify@BGPNotify{} -> do
               -- TODO - improve Notify analysis and display
               idle "stateConnected -> exit rcv notify"
            update@BGPUpdate{} -> do
                bgpSnd bsock $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateConnected - recvd Update - FSM error"
            z -> do
                idle $ "stateConnected - network exception - " ++ show z

    stateOpenSent :: F
    stateOpenSent (bsock,osm) = do
        (bsock',msg) <- get bsock initialHoldTimer
        case msg of 
          BGPTimeout -> do
              bgpSnd bsock $ BGPNotify NotificationHoldTimerExpired 0 L.empty
              idle "stateOpenSent - error initial Hold Timer expiry"
          open@BGPOpen{} -> do
              let osm' = updateOpenStateMachine osm open
                  resp =  getResponse osm'
              putStrLn "stateOpenSent - rcv open"
              collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
              if isJust collision then do
                  bgpSnd bsock $ BGPNotify NotificationCease 0 L.empty
                  idle (fromJust collision)
              else if isKeepalive resp then do 
                  bgpSnd bsock resp
                  putStrLn "stateOpenSent -> stateOpenConfirm"
                  return (StateOpenConfirm,bsock',osm')
              else idle "stateOpenSent - open rejected error"
          notify@BGPNotify{} -> do
             idle "stateOpenSent - rcv notify"
          _ -> do
              bgpSnd bsock $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
              idle "stateOpenSent - FSM error"

    stateOpenConfirm :: F
    stateOpenConfirm (bsock,osm) = do
        (bsock',msg) <- get bsock (getNegotiatedHoldTime osm)
        case msg of 
            BGPTimeout -> do
                bgpSnd bsock $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "stateOpenConfirm - error initial Hold Timer expiry"
            BGPKeepalive -> do
                putStrLn "stateOpenConfirm - rcv keepalive"
                return (ToEstablished,bsock',osm)
            notify@BGPNotify{} -> do
                idle "stateOpenConfirm - rcv notify"
            _ -> do
                bgpSnd bsock $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateOpenConfirm - FSM error"

    toEstablished :: F
    toEstablished (bsock,osm) = do
        putStrLn "transition -> established"
        putStrLn $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        let remoteBGPid = bgpID $ fromJust $ remoteOffer osm
            remoteAS = myAutonomousSystem $ fromJust $ remoteOffer osm
            peerData' = peerData { peerAS = fromIntegral remoteAS , peerBGPid = remoteBGPid }
        peerName <- getPeerName (rawSocket bsock)
        registerEstablished collisionDetector remoteBGPid peerName
        -- VERY IMPORTANT TO USE THE NEW VALUE peerData' AS THIS IS THE ONLY ONE WHICH CONTAINS ACCURATE REMOTE IDENTITY FOR DYNAMIC PEERS!!!!
        -- it would be much better to remove the temptation to use conficured data by forcing a new type for relevant purposes, and dscarding the
        -- preconfigured values as soon as possible
        addPeer rib peerData'
        forkIO $ keepAliveLoop bsock rib peerData'  (getKeepAliveTimer osm)
        return (Established,bsock,osm)

    established :: F
    established (bsock,osm) = do
        (bsock',msg) <- get bsock (getNegotiatedHoldTime osm)
        case msg of 
            BGPKeepalive -> do
                -- hFlush logFile
                return (Established,bsock',osm)
            update@BGPUpdate{} ->
                maybe
                    ( do
                         bgpSnd bsock $ BGPNotify NotificationUPDATEMessageError 0 L.empty
                         idle "established - Update parse error"
                    )
                    (\parsedUpdate -> do
                      -- TODO - don't use the now obselete value of peerData
                      -- which now should be peerData'
                      let routeData = Rib.makeRouteData peerData parsedUpdate
                      Rib.ribUpdater rib peerData parsedUpdate
                      return (Established,bsock',osm)
                    )
                    ( processUpdate update )

            notify@BGPNotify{} -> do
                idle "established - rcv notify"
            BGPEndOfStream -> idle "established: BGPEndOfStream"
            BGPTimeout -> do
                bgpSnd bsock $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "established - HoldTimerExpired error"
            _ -> do
                bgpSnd bsock $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "established - FSM error"

    -- collisionCheck
    -- manage cases where there is an established connection (always reject)
    -- and where another connection is in openSent state (use tiebreaker)
    -- and of couse where there is no other connection for this BGPID
    collisionCheck :: CollisionDetector -> IPv4 -> IPv4 -> IO (Maybe String)
    collisionCheck c self peer = do
        -- TODO - work out whether keeping the socket info is valuable, since we never use it
        --        for now fake it up since it is no longer in visibility
        let peerName = SockAddrInet 0 0
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

    keepAliveLoop bsock rib peer timer = do
        running <- catch
            ( do sendQueuedUpdates bsock rib peer timer
                 return True
            )
            (\(FSMException s) -> do
                -- this is perfectly normal event when the fsm closes down as it doesn't stop the keepAliveLoop explicitly
                return False
            )
        when running
            ( keepAliveLoop bsock rib peer timer )

-- TODO merge sendQueuedUpdates in keepAliveLoop?
    sendQueuedUpdates bsock rib peer timeout = do
        updates <- pullAllUpdates (1000000 * timeout) peer rib
        if null updates then
            bgpSnd bsock BGPKeepalive
        else do routes <- lookupRoutes rib peer updates
                putStr $ "Ready to send routes to " ++ show (peerIPv4 peer)
                if 11 > length updates then do
                    print $ map fst updates
                else do
                    print $ map fst (take 10 updates)
                    putStrLn $ "and " ++ show (length updates - 10) ++ " more"
                mapM_ (bgpSnd bsock) routes

getLogFile = do
    t <- utcSecs
    -- TODO make unique names because multiple peers may start at the same time....
    -- handle <- openBinaryFile ("trace/" ++ show t ++ ".bgp") WriteMode
    return Nothing
    -- return $ Just handle
