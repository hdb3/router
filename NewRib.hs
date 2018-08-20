{-# LANGUAGE RecordWildCards #-}
module NewRib where
import Data.IORef
import qualified Data.ByteString.Lazy as L

import BGPData
import Prefixes
import PrefixTable
import PrefixTableUtils
import PathAttributes

type Rib = IORef Rib'
data Rib' = Rib' { peer :: PeerData
                 , prefixTable :: PrefixTable
                 }

newRib :: IO Rib
newRib = newIORef newRib'
newRib' :: Rib'
newRib' = Rib' defaultPeerData newPrefixTable

getRib :: Rib -> IO PrefixTable
getRib rib = do
    rib' <- readIORef rib
    return (prefixTable rib')

ribUpdateMany :: Rib -> [PathAttribute] -> Int -> [Prefix] -> IO()
ribUpdateMany rib attrs hash pfxs = modifyIORef' rib (ribUpdateMany' attrs hash pfxs)
ribUpdateMany' :: [PathAttribute] -> Int -> [Prefix] -> Rib' -> Rib'
ribUpdateMany' attrs hash pfxs (Rib' peerData prefixTable ) = let
    ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
    routeData = RouteData peerData attrs hash pathLength nextHop origin med fromEBGP
    fromEBGP = isExternal peerData
    pathLength = getASPathLength attrs
    med = if fromEBGP then 0 else getMED attrs
    nextHop = getNextHop attrs
    origin = getOrigin attrs
    in Rib' peerData prefixTable'

ribWithdrawMany rib p = modifyIORef' rib (ribWithdrawMany' p)
ribWithdrawMany' :: [Prefix] -> Rib' -> Rib'
ribWithdrawMany' pfxs (Rib' peerData prefixTable ) = let
    ( prefixTable' , updates ) = PrefixTable.withdraw prefixTable (fromPrefixes pfxs) peerData
    in Rib' peerData prefixTable'
