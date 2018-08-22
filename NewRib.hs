{-# LANGUAGE RecordWildCards #-}
module NewRib where
import Data.IORef
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Data.Map
import Data.Maybe(fromJust)

import Common
import BGPData
import Prefixes
import PrefixTable
import qualified PrefixTableUtils
import PathAttributes
import AdjRIBOut

type Rib = IORef Rib'
type AdjRIB = Data.Map.Map PeerData AdjRIBTable
data Rib' = Rib' { 
                   prefixTable :: PrefixTable
                 , adjRib:: AdjRIB
                 }

newRib :: IO Rib
newRib = newIORef newRib'
newRib' :: Rib'
newRib' = Rib' newPrefixTable ( Data.Map.singleton defaultPeerData newAdjRIBTable ) 

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyIORef' rib ( addPeer' peer )

addPeer' ::  PeerData -> Rib' -> Rib'
-- addPeer' peer Rib' {..} = let adjRib' = Data.Map.insert peer newAdjRIBOut adjRib in Rib' prefixTable adjRib'
addPeer' peer Rib' {..} = let adjRib' = Data.Map.insert peer aro adjRib
                              aro = fifo $ map f $ PrefixTableUtils.getAdjRIBOut prefixTable
                              -- aro = newAdjRIBOut
                              f (rd,ipfxs) = (ipfxs , routeId rd)
                           in Rib' prefixTable adjRib'

-- overloaded in a very non-Haskell way - requesting zero updates actually returns everything!
pullUpdates :: Int -> PeerData -> Rib -> IO [AdjRIBEntry]
pullUpdates n peer rib = atomicModifyIORef' rib f where
    f (Rib' pt arot) = (Rib' pt arot' , updates) where
        (arot',updates) = pullUpdates' n peer arot

pullUpdates' :: Int -> PeerData -> AdjRIB -> (AdjRIB,[AdjRIBEntry])
pullUpdates' n peer aroTable = (aroTable',updates) where
    aroTable' = Data.Map.update (\_ -> Just aro') peer aroTable
    (aro',updates) | n == 0    = dequeueAll (aroTable Data.Map.! peer) 
                   | otherwise = dequeueN n (aroTable Data.Map.! peer) 

-- this is a read only operation which does not change the RIB
-- see pullUpdates for the operational function which empties the AdjRibOut fifo
getARO :: PeerData -> Rib -> IO [AdjRIBEntry]
getARO peer rib = do
    rib' <- readIORef rib
    return $ peekAllAdjRIBTable $ fromJust $ Data.Map.lookup peer (adjRib rib')

getRib :: Rib -> IO PrefixTable
getRib rib = do
    rib' <- readIORef rib
    return (prefixTable rib')

-- updateAdjRibOutTables -- this function applies the same update to _all_ of the adjribs
-- it is called from within ribUpdate so has no IO wrapper of its own
updateAdjRibOutTables :: AdjRIBEntry -> AdjRIB -> AdjRIB
updateAdjRibOutTables are = Data.Map.map ( insertAdjRIBTable are )

-- TODO - convert ribUpdateMany/ribWithdrawMany to IPrefix based, for consistency...
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
