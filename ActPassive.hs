-- passive TCP server
module Main where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (forever)
import Network.Socket
import System.IO(openBinaryFile,IOMode( WriteMode ))
import qualified Data.IP
import qualified Data.Map.Strict as Data.Map

import Common
import BGPData
import BgpFSM
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

start peers = do
    putStrLn "ActPassive starting"
    print peers

    let global = globalData (head peers)
        listenAddress = SockAddrInet bgpPort 0 -- listen on all intefaces by default...
        local = localPeer global
        peerMap = Data.Map.fromList $ map (\pd -> (peerIPv4 pd,pd)) peers
    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    sessions <- newMVar ( Data.Map.empty :: Data.Map.Map ThreadId (IPv4,PeerData) )
    rib <- Rib.newRib local
    insertStatic rib local
    let commons = (rib,peerMap,exitMVar,collisionDetector,sessions,global,local,listenAddress)
    putStrLn "ActPassive ready"

    forkIO $ reaper commons
    forkIO $ listener commons 
    mapM_ (connectTo commons) peers
    return ()

reaper commons = 
    let (rib,peerMap,exitMVar,collisionDetector,sessions,global,local,listenAddress) = commons
    in forever $ do
        putStrLn "reaper"
        (tid,peerName,es) <- takeMVar exitMVar
        either
            (\s -> putStrLn $ "thread " ++ show tid ++ "/" ++ show peerName ++ " exited with exception <" ++ s ++ ">")
            (\s -> putStrLn $ "thread " ++ show tid ++ "/" ++ show peerName ++ " exited normally <" ++ s ++ ">")
            es
        threadMap <- takeMVar sessions
        delPeer rib ( snd (threadMap Data.Map.! tid ))
        putMVar sessions ( Data.Map.delete tid threadMap )

listener commons = do
    let (_,peerMap,_,_,_,_,_,listenAddress) = commons
    sock <- socket AF_INET Stream defaultProtocol 
    setSocketOption sock ReuseAddr 1
    bind sock listenAddress
    listen sock 100
    forever $ do
        (conn, peerAddress) <- accept sock
        putStrLn "listener"
        maybe
            ( putStrLn $ "Reject connection from " ++ show peerAddress )
            ( \peerData -> startFSM (conn,peerAddress,peerData,commons))
            (getPeerConfig commons peerAddress)

defaultPeerData = Nothing -- change to permit unconfigured inbound connections
getPeerConfig commons peerAddress = maybe defaultPeerData Just (getPeerConfig commons peerAddress)
getPeerConfig' (_,peerMap,_,_,_,_,_,_) peerAddress = Data.Map.lookup (getIPv4 peerAddress) peerMap


connectTo commons peerData = do
    conn <- socket AF_INET Stream defaultProtocol
    Network.Socket.connect conn ( SockAddrInet bgpPort (toHostAddress $ peerIPv4 peerData))
    peerName <- getPeerName conn
    putStrLn $ "connected outbound to: " ++ (show peerName)
    startFSM (conn,peerName,peerData,commons)

startFSM (conn,peerAddress,peerData,commons) = do
    let (rib,peerMap,exitMVar,collisionDetector,sessions,global,local,listenAddress) = commons
    putStrLn $ "starting session for " ++ show (getIPv4 peerAddress)
    logfile <- getLogFile
    let delayOpenTimer = 10
    let config = BgpFSMconfig conn collisionDetector peerAddress delayOpenTimer exitMVar logfile peerData rib
    threadId <- forkIO (bgpFSM config)
    threadMap <- takeMVar sessions
    putMVar sessions ( Data.Map.insert threadId ((getIPv4 peerAddress),peerData) threadMap )

getIPv4 :: SockAddr -> Data.IP.IPv4
getIPv4 (SockAddrInet portNumber hostAddress) = fromHostAddress hostAddress

getLogFile = do
    t <- utcSecs
    handle <- openBinaryFile ("trace/" ++ show t ++ ".bgp") WriteMode
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
