{-# LANGUAGE RecordWildCards #-}
module Rib where
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
import PathAttributeUtils
import Update
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

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = modifyIORef' rib ( delPeer' peer )

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyIORef' rib ( addPeer' peer )

delPeer' ::  PeerData -> Rib' -> Rib'
delPeer' peer Rib' {..} = Rib' prefixTable (Data.Map.delete peer adjRib)
-- this removes the peers adjRib but it does not clean up the loc-rib and force all the corresponding withdraws

addPeer' ::  PeerData -> Rib' -> Rib'
-- addPeer' peer Rib' {..} = let adjRib' = Data.Map.insert peer newAdjRIBOut adjRib in Rib' prefixTable adjRib'
addPeer' peer Rib' {..} = let adjRib' = Data.Map.insert peer aro adjRib
                              aro = fifo $ map f $ PrefixTableUtils.getAdjRIBOut prefixTable
                              -- aro = newAdjRIBOut
                              f (rd,ipfxs) = (ipfxs , routeId rd)
                              -- TODO - this would be the place to insert an end-of-rib marker
                           in Rib' prefixTable adjRib'

-- queryPrefixTable :: PrefixTable -> IPrefix -> Maybe RouteData
queryRib :: Rib -> IPrefix -> IO (Maybe RouteData)
queryRib rib prefix = do
    rib' <- readIORef rib
    return $ queryPrefixTable (prefixTable rib') prefix 

-- overloaded in a very non-Haskell way - requesting zero updates actually returns everything!
pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates = pullUpdates 0
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
peekUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
peekUpdates peer rib = do
    rib' <- readIORef rib
    return $ peekAllAdjRIBTable $ fromJust $ Data.Map.lookup peer (adjRib rib')

getRib :: Rib -> IO PrefixTable
getRib rib = do
    rib' <- readIORef rib
    return (prefixTable rib')

-- updateAdjRibOutTables -- this function applies the same update to _all_ of the adjribs
-- it is called from within ribUpdate so has no IO wrapper of its own
-- updateAdjRibOutTables :: AdjRIBEntry -> AdjRIB -> AdjRIB
-- updateAdjRibOutTables are = Data.Map.map ( insertAdjRIBTable are )

updateRibOutWithPeerData :: PeerData -> RouteData -> [IPrefix] -> AdjRIB -> AdjRIB
-- NOTE!!!! - we can be called with a null route in which case only the routeId is defined, and is equal 0!!!
-- this is OK since we only get the routeId in this function

updateRibOutWithPeerData originPeer routeData updates = Data.Map.mapWithKey updateWithKey where
    updateWithKey destinationPeer table = if isExternal destinationPeer || isExternal originPeer
                               then insertAdjRIBTable (updates, routeId routeData ) table
                               else table

makeRouteData :: PeerData -> ParsedUpdate -> RouteData
makeRouteData peerData parsedUpdate = makeRouteData' peerData ( puPathAttributes parsedUpdate) ( hash parsedUpdate)


makeRouteData' :: PeerData -> [PathAttribute] -> Int -> RouteData
makeRouteData' peerData pathAttributes routeId = RouteData peerData pathAttributes routeId pathLength nextHop origin med fromEBGP
    where
    pathLength = getASPathLength pathAttributes
    fromEBGP = isExternal peerData
    med = if fromEBGP then 0 else getMED pathAttributes
    nextHop = getNextHop pathAttributes
    origin = getOrigin pathAttributes

ribUpdater2 :: Rib -> PeerData -> ParsedUpdate -> IO()
ribUpdater2 rib routeData update = modifyIORef' rib (ribUpdater2' routeData update)

ribUpdater2' :: PeerData -> ParsedUpdate -> Rib' -> Rib'
ribUpdater2' peerData ParsedUpdate{..} = ribUpdateMany' peerData puPathAttributes hash nlri . ribWithdrawMany' peerData withdrawn

-- TODO remove ribUpdater and rename ribUpdater2 -> ribUpdater
ribUpdater :: Rib -> RouteData -> ParsedUpdate -> IO()
ribUpdater rib routeData update = modifyIORef' rib (ribUpdater' routeData update)

ribUpdater' :: RouteData -> ParsedUpdate -> Rib' -> Rib'
ribUpdater' RouteData{..} ParsedUpdate{..} = ribUpdateMany' peerData pathAttributes routeId nlri . ribWithdrawMany' peerData withdrawn

-- TODO - convert ribUpdateMany/ribWithdrawMany to IPrefix based, for consistency...
ribUpdateMany :: Rib -> PeerData -> [PathAttribute] -> Int -> [Prefix] -> IO()
ribUpdateMany rib peerData attrs hash pfxs = modifyIORef' rib (ribUpdateMany' peerData attrs hash pfxs)
ribUpdateMany' :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> Rib'
ribUpdateMany' peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables ) = let
    routeData = makeRouteData' peerData pathAttributes routeId
    ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
    adjRibOutTables' = updateRibOutWithPeerData peerData routeData updates adjRibOutTables
    in Rib' prefixTable' adjRibOutTables'

--ribWithdrawMany rib peer p = modifyIORef' rib (ribWithdrawMany' peer p)
ribWithdrawMany' :: PeerData -> [Prefix] -> Rib' -> Rib'
ribWithdrawMany' peerData pfxs (Rib' prefixTable adjRibOutTables) = let
    ( prefixTable' , updates ) = PrefixTable.withdraw prefixTable (fromPrefixes pfxs) peerData
    -- adjRibOutTables' = updateAdjRibOutTables (updates,0) adjRibOutTables
    adjRibOutTables' = updateRibOutWithPeerData peerData nullRoute updates adjRibOutTables
    in Rib' prefixTable' adjRibOutTables
