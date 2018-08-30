{-#LANGUAGE OverloadedStrings #-}
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
import BGPData
import Update
import Capabilities
import Args
import Collision
import Rib

main :: IO ()
main = do config <- getConfig
          either putStrLn
                 main'
                 config

main' (address,peerData) = do
    let global = globalData peerData
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
    rib <- Rib.newRib
    -- this is kludgy - the global and peer data distribution needs to be overhauled
    let local = localPeer global
        update = igpUpdate (myBGPid global) ["10.0.0.0/8","13.0.0.0/8"]
    ribUpdater2 rib local update
    let config = BgpFSMconfig sock collisionDetector peerName delayOpenTimer exitMVar (Just handle) peerData rib
    finally (bgpFSM config) (close sock) 
    (tid,msg) <- takeMVar exitMVar
    putStrLn $ "complete:: " ++ show (tid :: ThreadId) ++ " : " ++ msg
