{-# LANGUAGE DuplicateRecordFields,RecordWildCards #-}
module Main where

import Network.Socket
import Session
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map

import Config
import BgpFSM
import Collision
import BGPRib
import Global
import Redistributor

main :: IO ()
main = do
    configString <- readFile "bgp.conf"
    let rawConfig = read configString :: Config
    print rawConfig
    let config = buildPeerConfigs rawConfig
    print config

    putStrLn "Router starting"

    global <- buildGlobal config

    forkIO (redistribute global)
    
    let
        app = bgpFSM global

    putStrLn $ "connecting to " ++ show (activePeers config)
    session 179 app (activePeers config)
    putStrLn "Router ready"
    idle where idle = do threadDelay 10000000
                         idle

buildGlobal c@Config{..} = do
    let
        config = c
        gd = GlobalData { myAS = configAS , myBGPid = configBGPID }
        ld = localPeer gd
        delayOpenTimer = configDelayOpenTimer
        initialHoldTimer = configInitialHoldTimer

        -- TODO  - configure this in configuration file
        listenAddress = SockAddrInet 179 0 -- listen on all intefaces by default...

        -- TODO the map creation should be in Config...
        peerMap = Data.Map.fromList $ map (\pc -> (peerConfigIPv4 pc,pc)) configConfiguredPeers

    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- BGPRib.newRib ld
    return Global {..}
