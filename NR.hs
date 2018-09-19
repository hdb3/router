{-# LANGUAGE DuplicateRecordFields,RecordWildCards #-}
module Main where

import Network.Socket
-- import qualified Data.IP
import Session
-- import Data.Char(toUpper)
import Control.Concurrent
-- import qualified Control.Exception as E
-- import Control.Monad (when,forever)
-- import System.IO(openBinaryFile,IOMode( WriteMode ))
import qualified Data.Map.Strict as Data.Map

import Config
import Common
import BGPData
import BgpFSM
import Collision
-- import Args2
import Rib
import BGPReader(pathReadRib)
import Update(makeUpdate)
import Global
import Capabilities

main :: IO ()
main = do
    configString <- readFile "bgp.conf"
    let rawConfig = read configString :: Config
    print rawConfig
    let config = buildPeerConfigs rawConfig
    print config

    putStrLn "Router starting"

    global <- buildGlobal config
    
    let
        app = bgpFSM global

    putStrLn $ "connecting to " ++ show (activePeers config)
    session 179 app (activePeers config)
    putStrLn "Router ready"
    -- threadDelay 30000000
    -- putStrLn "insert routes"
    -- insertStatic (rib global) (ld global)
    -- putStrLn "done"
    idle where idle = do threadDelay 10000000
                         idle

insertStatic rib local = do
    -- pathReadRib :: FilePath -> IO [((Int, [PathAttributes.PathAttribute]), [Prefixes.Prefix])]
    updates <- pathReadRib "bgpdata/full.bgp"
    -- ribUpdater :: Rib -> PeerData -> ParsedUpdate -> IO()
    -- makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> ParsedUpdate
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) (take 1000000 updates)
    -- mapM print updates'
    mapM (ribUpdater rib local) updates'
    return ()

buildGlobal c@Config{..} = do
    let --TODO - consider simply sending most of the Config on the Global record directly - it is already parsed...
        config = c
        gd = GlobalData { myAS = configAS , myBGPid = configBGPID }
        ld = localPeer gd
        holdTime = configOfferedHoldTime
        delayOpenTimer = configDelayOpenTimer
        initialHoldTimer = configInitialHoldTimer

        -- TODO  - configure this in configuration file
        listenAddress = SockAddrInet bgpPort 0 -- listen on all intefaces by default...

        configuredPeers = configEnabledPeers

        -- TODO the map creation should be in Config...
        peerMap = Data.Map.fromList $ map (\pc -> (peerConfigIPv4 pc,pc)) configConfiguredPeers

        -- defaultPeerData = Just $ buildDefaultPeerData gd holdTime
        
    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- Rib.newRib ld
    return Global {..}
