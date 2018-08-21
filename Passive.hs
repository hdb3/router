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
import System.Timeout
import Data.Int(Int64)

import Common
import BGPparse
import BGPData
import GetBGPMsg
import BgpFSM
import Capabilities
import Collision
import Args

main :: IO ()
main = do config <- getConfig
          either putStrLn
                 main'
                 config

main' (address,local,remote,peerData) = do
    putStrLn "Passive starting"
    print (address,local,remote)
    sock <- socket AF_INET Stream defaultProtocol 
    setSocketOption sock ReuseAddr 1
    bind sock address
    listen sock 100
    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    forkIO $ reaper exitMVar
    let config = BgpFSMconfig local remote undefined collisionDetector undefined delayOpenTimer exitMVar Nothing peerData
    E.finally (loop sock config) (close sock)
  where
    delayOpenTimer = 10
    reaper mbox = forever $ do
        (t,s) <- takeMVar mbox
        putStrLn $ "thread " ++ show t ++ " exited with <" ++ s ++ ">"
    loop sock config = forever $ do
        (conn, peer) <- accept sock
        let config' = config { sock = conn, peerName = peer}
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (bgpFSM config') (\tid -> do putStrLn $ "ending " ++ show tid
                                                        close conn)
