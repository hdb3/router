{-# LANGUAGE RecordWildCards #-}
module NewRib where
import Data.IORef
import qualified Data.ByteString.Lazy as L

import BGPData
import Prefixes
import PrefixTable
import PrefixTableUtils
import PathAttributes
import AdjRIBOut

type Rib = IORef Rib'
data Rib' = Rib' { peer :: PeerData
                 , prefixTable :: PrefixTable
                 , adjRibOuts  :: [AdjRIBOut]
                 }

newRib :: IO Rib
newRib = newIORef newRib'
newRib' :: Rib'
-- test case with multiple peers
-- newRib' = Rib' defaultPeerData newPrefixTable ( replicate 10 newAdjRIBOut )
-- test case with zero peers
-- newRib' = Rib' defaultPeerData newPrefixTable []
newRib' = Rib' defaultPeerData newPrefixTable [newAdjRIBOut]

getARO :: Rib -> IO [AdjRIBEntry]
getARO rib = do
    rib' <- readIORef rib
    return ( peekAllAdjRIBOut $ head $ adjRibOuts rib')

getRib :: Rib -> IO PrefixTable
getRib rib = do
    rib' <- readIORef rib
    return (prefixTable rib')

updateAdjRibOutTables :: AdjRIBEntry -> [AdjRIBOut] -> [AdjRIBOut]
updateAdjRibOutTables are = map ( insertAdjRIBOut are )

ribUpdateMany :: Rib -> [PathAttribute] -> Int -> [Prefix] -> IO()
ribUpdateMany rib attrs hash pfxs = modifyIORef' rib (ribUpdateMany' attrs hash pfxs)
ribUpdateMany' :: [PathAttribute] -> Int -> [Prefix] -> Rib' -> Rib'
ribUpdateMany' attrs hash pfxs (Rib' peerData prefixTable adjRibOutTables ) = let
    ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
    routeData = RouteData peerData attrs hash pathLength nextHop origin med fromEBGP
    fromEBGP = isExternal peerData
    pathLength = getASPathLength attrs
    med = if fromEBGP then 0 else getMED attrs
    nextHop = getNextHop attrs
    origin = getOrigin attrs
    adjRibOutTables' = updateAdjRibOutTables (updates,hash) adjRibOutTables
    in Rib' peerData prefixTable' adjRibOutTables'

ribWithdrawMany rib p = modifyIORef' rib (ribWithdrawMany' p)
ribWithdrawMany' :: [Prefix] -> Rib' -> Rib'
ribWithdrawMany' pfxs (Rib' peerData prefixTable  adjRibOutTables) = let
    ( prefixTable' , updates ) = PrefixTable.withdraw prefixTable (fromPrefixes pfxs) peerData
    adjRibOutTables' = updateAdjRibOutTables (updates,0) adjRibOutTables
    in Rib' peerData prefixTable' adjRibOutTables
