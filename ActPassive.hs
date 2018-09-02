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

    putStrLn "Passive starting"
    sock <- socket AF_INET Stream defaultProtocol 
    setSocketOption sock ReuseAddr 1
    bind sock address
    listen sock 100
    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    forkIO $ reaper exitMVar
    rib <- Rib.newRib
    E.finally (loop (sock,rib,peerMap,exitMVar,collisionDetector) )
              (close sock)
  where
    delayOpenTimer = 10
    reaper mbox = forever $ do
        (t,s) <- takeMVar mbox
        putStrLn $ "thread " ++ show t ++ " exited with <" ++ s ++ ">"
    loop (sock,rib,peerMap,exitMVar,collisionDetector) = forever $ do
        (conn, peer) <- accept sock
        let peerIPv4 = getIPv4 peer
        maybe
            ( putStrLn $ "Reject connection from " ++ show peer )
            ( \peerData -> do
                putStrLn $ "Connection from " ++ show peer
                logfile <- getLogFile
                let config = BgpFSMconfig conn collisionDetector peer delayOpenTimer exitMVar logfile peerData rib
                void $ forkFinally (bgpFSM config)
                                   (\tid -> do putStrLn $ "ending " ++ show tid
                                               close conn
                                   )
            )
            ( Data.Map.lookup peerIPv4 peerMap )


getIPv4 :: SockAddr -> Data.IP.IPv4
getIPv4 (SockAddrInet portNumber hostAddress) = fromHostAddress hostAddress

getLogFile = do
    t <- utcSecs
    handle <- openBinaryFile (show t ++ ".bgp") WriteMode
    return $ Just handle
