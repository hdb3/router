{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (when,forever)
import Network.Socket
import System.IO(openBinaryFile,IOMode( WriteMode ))
import qualified Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error(catchIOError)

import Common
import BGPData
import BgpFSM hiding (exitMVar)
import Collision
import Args2
import Rib
import BGPReader(pathReadRib)
import Update(makeUpdate)

main :: IO ()
main = do
    peers <- getConfig
    either
        putStrLn
        start
        peers

idleTimeout = 60 * 1000000

data Commons = Commons { rib :: Rib.Rib
                     , peerMap :: Data.Map.Map IPv4 PeerData
                     ,exitMVar :: MVar (ThreadId, SockAddr, Either String String)
                     ,collisionDetector :: CollisionDetector
                     ,sessions :: MVar ( Data.Map.Map ThreadId PeerData )
                     ,global :: GlobalData
                     ,local :: PeerData
                     ,listenAddress :: SockAddr
                     }
start peers = do
    putStrLn "ActPassive starting"
    print peers

    let global = globalData (head peers)
        listenAddress = SockAddrInet bgpPort 0 -- listen on all intefaces by default...
        local = localPeer global
        configuredPeers = map peerIPv4 peers
        peerMap = Data.Map.fromList $ map (\pd -> (peerIPv4 pd,pd)) peers
    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    sessions <- newMVar Data.Map.empty
    rib <- Rib.newRib local
    insertStatic rib local
    let commons = Commons {..}
    putStrLn "ActPassive ready"

    forkIO $ reaper commons
    forkIO $ listener commons 
    mapM_ (forkIO . connectImmediate commons) peers
    putStrLn "ActPassive running"
    idle

idle = do
    threadDelay 100000000 -- 1 second spin
                            -- could put some diagnstics here if wanted.......
    idle

reaper commons@Commons{..} = 
    forever $ do
        -- putStrLn "reaper"
        (tid,peerAddress,es) <- takeMVar exitMVar
        either
            (\s -> putStrLn $ "thread " ++ show tid ++ "/" ++ show peerAddress ++ " exited,\n with exception <" ++ s ++ ">")
            (\s -> putStrLn $ "thread " ++ show tid ++ "/" ++ show peerAddress ++ " exited,\n after <" ++ s ++ ">")
            es
        threadMap <- takeMVar sessions
        -- TODO
        -- delegate the applicaion specific cleanup (delPeer) to the application, i.e. in BGPfsm
        -- but then it should also be initialised there too
        let peerData = threadMap Data.Map.! tid
        -- TODO make this all a modify MVar...
        putMVar sessions ( Data.Map.delete tid threadMap )
        delPeer rib peerData

        -- this is for a duplicate check in case the peer started a parallel session whilst one was already in progress
        activePeers <- fmap ( map peerIPv4 . Data.Map.elems ) (readMVar sessions)

        let peerIP = getIPv4 peerAddress
            configuredPeerIPs = Data.Map.keys peerMap
            isConfiguredPeer = peerIP `elem` configuredPeerIPs
            parallelPeer = peerIP `elem` activePeers
        if isConfiguredPeer then
            if parallelPeer then
                putStrLn $ "*** NOT Rescheduling FSM for " ++ show (getIPv4 peerAddress)  ++ "because of duplicate session"
            else do
                putStrLn $ "Rescheduling FSM for " ++ show (getIPv4 peerAddress)
                forkIO (connectTo 10 commons peerData)
                return ()
        else
            putStrLn $ "*** NOT Rescheduling FSM for " ++ show (getIPv4 peerAddress)  ++ "because no configured peer at this address"

listener commons@Commons{..} = do
    putStrLn "listener"
    sock <- socket AF_INET Stream defaultProtocol 
    setSocketOption sock ReuseAddr 1
    bind sock listenAddress
    listen sock 100
    forever $ do
        (conn, peerAddress) <- accept sock
        putStrLn $ "listener - connection from " ++ show peerAddress
        maybe
            ( putStrLn "Reject" )
            ( \peerData -> do putStrLn "Accept"
                              startFSM commons (conn,peerAddress,peerData))
            (getPeerConfig commons peerAddress)

defaultPeerData = Nothing -- change to permit unconfigured inbound connections
getPeerConfig commons peerAddress = maybe defaultPeerData Just (getPeerConfig' commons peerAddress)
getPeerConfig' Commons{..} peerAddress = Data.Map.lookup (getIPv4 peerAddress) peerMap

connectImmediate = connectTo 0
connectTo delay commons@Commons{..} peerData = do
    threadDelay $ delay * 1000000
    let remoteAddress = SockAddrInet bgpPort (toHostAddress $ peerIPv4 peerData)
    tid <- myThreadId
    catchIOError ( do
        conn <- socket AF_INET Stream defaultProtocol
        Network.Socket.connect conn remoteAddress
        peerAddress <- getPeerName conn
        putStrLn $ "connected outbound to: " ++ (show peerAddress)
        startFSM commons (conn,peerAddress,peerData)
        )
        (\e -> do -- can get rid of error to screen if the response is displayed elsewhere
            -- putStrLn $ "IOError in connectTo: " ++ show (e :: IOError)
            threadDelay idleTimeout
            modifyMVar_ sessions (\threadMap -> return ( Data.Map.insert tid peerData threadMap) )
            putMVar exitMVar (tid , remoteAddress, Left $ show (e :: IOError) )
        )

startFSM Commons{..} (conn,peerAddress,peerData) = do
    putStrLn $ "starting session for " ++ show (getIPv4 peerAddress)
    logfile <- getLogFile
    let delayOpenTimer = 10
    let config = BgpFSMconfig conn collisionDetector peerAddress delayOpenTimer exitMVar logfile peerData rib
    threadId <- forkIO (bgpFSM config)
    threadMap <- takeMVar sessions
    putMVar sessions ( Data.Map.insert threadId peerData threadMap )

getIPv4 (SockAddrInet portNumber hostAddress) = fromHostAddress hostAddress

getLogFile = do
    t <- utcSecs
    -- TODO make unique names because multiple peers may start at the same time....
    -- handle <- openBinaryFile ("trace/" ++ show t ++ ".bgp") WriteMode
    return Nothing
    -- return $ Just handle

insertStatic rib local = do
    -- pathReadRib :: FilePath -> IO [((Int, [PathAttributes.PathAttribute]), [Prefixes.Prefix])]
    updates <- pathReadRib "bgpdata/full.bgp"
    -- ribUpdater :: Rib -> PeerData -> ParsedUpdate -> IO()
    -- makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> ParsedUpdate
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) (take 1000 updates)
    -- mapM print updates'
    -- mapM (ribUpdater rib local) updates'
    return ()
