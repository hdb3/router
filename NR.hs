{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.Socket
import qualified Data.IP
import Session
import Data.Char(toUpper)
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
import Global

main :: IO ()
main = do
    peers <- getConfig
    either
        putStrLn
        start
        peers

start peers = do
    putStrLn "Router starting"
    print peers

    let gd = globalData (head peers)
        listenAddress = SockAddrInet bgpPort 0 -- listen on all intefaces by default...
        ld = localPeer gd
        configuredPeers = map peerIPv4 peers
        peerMap = Data.Map.fromList $ map (\pd -> (peerIPv4 pd,pd)) peers
        delayOpenTimer = 3

    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    sessions <- newMVar Data.Map.empty
    rib <- Rib.newRib ld
    insertStatic rib ld

    let global = Global {..}
        app = bgpFSM global

    session 179 app configuredPeers
    putStrLn "Router ready"
    idle where idle = do threadDelay 10000000
                         idle

insertStatic rib local = do
    -- pathReadRib :: FilePath -> IO [((Int, [PathAttributes.PathAttribute]), [Prefixes.Prefix])]
    updates <- pathReadRib "bgpdata/full.bgp"
    -- ribUpdater :: Rib -> PeerData -> ParsedUpdate -> IO()
    -- makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> ParsedUpdate
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) (take 1000 updates)
    -- mapM print updates'
    -- mapM (ribUpdater rib local) updates'
    return ()
