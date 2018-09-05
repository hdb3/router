-- passive TCP server
module Main where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, send)
import Data.Binary(encode,decode)
import System.IO(Handle,openBinaryFile,IOMode( WriteMode ))
import System.Timeout
import Data.Int(Int64)
import qualified Data.IP
import qualified Data.Map.Strict as Data.Map

import Common
import BGPparse
import BGPData
import GetBGPMsg
import BgpFSM
import Capabilities
import Collision
import Args2
import Rib
import BGPReader
import Update

main :: IO ()
main = do config <- getConfig
          either putStrLn
                 main'
                 config

main' peers = do
    print peers

    let global = globalData (head peers)
        address = SockAddrInet bgpPort 0 -- listen on all intefaces by default...
        local = localPeer global

    let peerMap = Data.Map.fromList $ map (\pd -> (peerIPv4 pd,pd)) peers
    print peerMap

    putStrLn "ActPassive starting"
    sock <- socket AF_INET Stream defaultProtocol 
    setSocketOption sock ReuseAddr 1
    bind sock address
    listen sock 100
    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    sessions <- newMVar ( Data.Map.empty :: Data.Map.Map ThreadId PeerData )
    rib <- Rib.newRib
    insertStatic rib local
    putStrLn "ActPassive ready"
    forkIO $ reaper exitMVar rib sessions
    E.finally (loop (sock,rib,peerMap,exitMVar,collisionDetector,sessions) )
              (close sock)

reaper mbox rib sessions = forever $ do
    (tid,peerName,es) <- takeMVar mbox
    either
        (\s -> putStrLn $ "thread " ++ show tid ++ "/" ++ show peerName ++ " exited with exception <" ++ s ++ ">")
        (\s -> putStrLn $ "thread " ++ show tid ++ "/" ++ show peerName ++ " exited normally <" ++ s ++ ">")
        es
    threadMap <- takeMVar sessions
    -- let peerData = threadMap ! tid
    delPeer rib ( threadMap Data.Map.! tid )
    putMVar sessions ( Data.Map.delete tid threadMap )


loop (sock,rib,peerMap,exitMVar,collisionDetector,sessions) = forever $ do
    (conn, peer) <- accept sock
    let peerIPv4 = getIPv4 peer
        delayOpenTimer = 10
    maybe
        ( putStrLn $ "Reject connection from " ++ show peer )
        ( \peerData -> do
            putStrLn $ "Connection from " ++ show peer
            logfile <- getLogFile
            let config = BgpFSMconfig conn collisionDetector peer delayOpenTimer exitMVar logfile peerData rib
            threadId <- forkIO (bgpFSM config)
            threadMap <- takeMVar sessions
            putMVar sessions ( Data.Map.insert threadId peerData threadMap )
        )
        ( Data.Map.lookup peerIPv4 peerMap )


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
    -- ribUpdater2 :: Rib -> PeerData -> ParsedUpdate -> IO()
    -- makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> ParsedUpdate
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) (take 1000 updates)
    -- mapM print updates'
    -- mapM (ribUpdater2 rib local) updates'
    return ()
