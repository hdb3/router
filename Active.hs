module Main where
-- active TCP speaker
import System.Environment
import Control.Exception(finally)
import Control.Concurrent
import Network.Socket
import System.IO(Handle,openBinaryFile,IOMode( WriteMode ))

import Common
import BgpFSM
import BGPparse
import Capabilities
import Args
import Collision
import NewRib

main :: IO ()
main = do config <- getConfig
          either putStrLn
                 main'
                 config

main' (address,peerData) = do
    print address
    putStrLn "begin:: "
    sock <- socket AF_INET Stream defaultProtocol
    connect sock address
    putStrLn "connected:: "
    collisionDetector <- mkCollisionDetector
    peerName <- getPeerName sock
    let delayOpenTimer = 0
    exitMVar <- newEmptyMVar
    t <- utcSecs
    handle <- openBinaryFile (show t ++ ".bgp") WriteMode
    rib <- NewRib.newRib
    let config = BgpFSMconfig sock collisionDetector peerName delayOpenTimer exitMVar (Just handle) peerData rib
    finally (bgpFSM config) (close sock) 
    (tid,msg) <- takeMVar exitMVar
    putStrLn $ "complete:: " ++ show (tid :: ThreadId) ++ " : " ++ msg
