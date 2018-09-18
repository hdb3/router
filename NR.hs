{-# LANGUAGE RecordWildCards #-}
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

import Common
import BGPData
import BgpFSM
import Collision
import Args2
import Rib
import BGPReader(pathReadRib)
import Update(makeUpdate)
import Global
import Capabilities

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

    -- note that the global data is the same in every peer relcord - we simply use the first in the list
    let gd = globalData (head peers)
        holdTime = propHoldTime (head peers)
    global <- buildGlobal gd peers holdTime

    insertStatic rib ld

    let
        app = bgpFSM global
        configuredPeers = map peerIPv4 peers

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

buildDefaultPeerData gd holdTime = PeerData { globalData = gd
                                   ,  isExternal = undefined
                                   ,  peerAS = 0
                                   ,  peerBGPid = fromHostAddress 0
                                   ,  peerIPv4 = undefined
                                   ,  localIPv4 = undefined
                                   ,  localPref = 0
                                   ,  propHoldTime = holdTime
                                   ,  reqHoldTime = holdTime
                                   ,  offerCapabilies = [ CapAS4 (myAS gd) ]
                                   ,  requireCapabilies = [ CapAS4 0 ] -- note that the effect here is to demand that CapAS4 is present, without specifying the peer AS expected
                                   }



buildGlobal gd peers holdTime = do
    let
        listenAddress = SockAddrInet bgpPort 0 -- listen on all intefaces by default...
        ld = localPeer gd
        configuredPeers = map peerIPv4 peers
        peerMap = Data.Map.fromList $ map (\pd -> (peerIPv4 pd,pd)) peers
        delayOpenTimer = 3 -- how long to wair for an Open before sending one ourselves - avoid collisions in simple cases - probably should only be used on incoming sessions (TODO)
        initialHoldTimer = 100 -- note this is not the same as generic holdTime - this is how long to wait before aborting the initial OPEN exchange - RFC4271 reccommends 300 secs I think??
        defaultPeerData = Just $ buildDefaultPeerData gd holdTime
        
    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- Rib.newRib ld
    return Global {..}
