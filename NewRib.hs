{-# LANGUAGE RecordWildCards #-}
module NewRib where
import Data.IORef
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Data.Map
import Data.Maybe(fromJust)

import Common
import BGPData
import Prefixes
import PrefixTable
import PrefixTableUtils
import PathAttributes
import AdjRIBOut

type Rib = IORef Rib'
type AdjRIB = Data.Map.Map PeerData AdjRIBOut
data Rib' = Rib' { 
                   prefixTable :: PrefixTable
                 , adjRib:: AdjRIB
                 }

newRib :: IO Rib
newRib = newIORef newRib'
newRib' :: Rib'
newRib' = Rib' newPrefixTable ( Data.Map.singleton defaultPeerData newAdjRIBOut ) 

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyIORef' rib ( addPeer' peer )

addPeer' ::  PeerData -> Rib' -> Rib'
-- addPeer' peer Rib' {..} = let adjRib' = Data.Map.insert peer newAdjRIBOut adjRib in Rib' prefixTable adjRib'
addPeer' peer Rib' {..} = let adjRib' = Data.Map.insert peer aro adjRib
                              aro = AdjRIBOut $ fifo $ map f $ getAdjRIBOut prefixTable
                              -- aro = newAdjRIBOut
                              f (rd,ipfxs) = (ipfxs , routeId rd)
                           in Rib' prefixTable adjRib'

getARO :: PeerData -> Rib -> IO [AdjRIBEntry]
getARO peer rib = do
    rib' <- readIORef rib
    return $ peekAllAdjRIBOut $ fromJust $ Data.Map.lookup peer (adjRib rib')

getRib :: Rib -> IO PrefixTable
getRib rib = do
    rib' <- readIORef rib
    return (prefixTable rib')

updateAdjRibOutTables :: AdjRIBEntry -> AdjRIB -> AdjRIB
updateAdjRibOutTables are = Data.Map.map ( insertAdjRIBOut are )

ribUpdateMany :: Rib -> PeerData -> [PathAttribute] -> Int -> [Prefix] -> IO()
ribUpdateMany rib peerData attrs hash pfxs = modifyIORef' rib (ribUpdateMany' peerData attrs hash pfxs)
ribUpdateMany' :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> Rib'
ribUpdateMany' peerData attrs hash pfxs (Rib' prefixTable adjRibOutTables ) = let
    ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
    routeData = RouteData peerData attrs hash pathLength nextHop origin med fromEBGP
    fromEBGP = isExternal peerData
    pathLength = getASPathLength attrs
    med = if fromEBGP then 0 else getMED attrs
    nextHop = getNextHop attrs
    origin = getOrigin attrs
    adjRibOutTables' = updateAdjRibOutTables (updates,hash) adjRibOutTables
    in Rib' prefixTable' adjRibOutTables'

ribWithdrawMany rib peer p = modifyIORef' rib (ribWithdrawMany' peer p)
ribWithdrawMany' :: PeerData -> [Prefix] -> Rib' -> Rib'
ribWithdrawMany' peerData pfxs (Rib' prefixTable adjRibOutTables) = let
    ( prefixTable' , updates ) = PrefixTable.withdraw prefixTable (fromPrefixes pfxs) peerData
    adjRibOutTables' = updateAdjRibOutTables (updates,0) adjRibOutTables
    in Rib' prefixTable' adjRibOutTables
