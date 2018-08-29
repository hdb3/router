module PrefixTable where

{- A single prefix table holds everything about a prefix we could care about
 - but, this is merely the prefix itself, and the associated path
 -
 - for IPv4 the prefix including length fits in a 64 bit word, so can be the actual key
 - though it might be that a simple scarmable operation would make a better key for a tree...
 - Note also that the pathtable key is also a 64 bit word, so a map of Ints is all that is required....
 -
 - However, the LocRIB needs to access every prefix table when performing selection
 -
 - Note: the route selection algorithm is at the heart of this system, and is performed for every prefix inserted
 - hence a fast implementation is essential
-}

import Data.IntMap.Strict(IntMap(),empty,insertLookupWithKey,toList,updateLookupWithKey)
import qualified Data.SortedList as SL -- package sorted-list
import qualified Data.List
import qualified Data.Tuple
import Data.IP

import Common
import BGPData
import Prefixes (IPrefix(..))

type PrefixTableEntry = SL.SortedList RouteData 
type PrefixTable = IntMap PrefixTableEntry

newPrefixTable :: PrefixTable
newPrefixTable = Data.IntMap.Strict.empty

slHead sl = x where
    Just (x,_) = SL.uncons sl

update:: PrefixTable -> [IPrefix] -> RouteData -> (PrefixTable,[IPrefix])
update pt pfxs route = Data.List.foldl' f (pt,[]) pfxs where
    f (pt_,updated) pfx = if p then (pt__,pfx:updated) else (pt__,updated) where
        (pt__,p) = updatePrefixTable pt_ pfx route

updatePrefixTable :: PrefixTable -> IPrefix -> RouteData -> (PrefixTable,Bool)
updatePrefixTable pt (IPrefix ipfx) route = (newPrefixTable, isNewBestRoute) where 
    updatePrefixTableEntry :: PrefixTableEntry -> PrefixTableEntry -> PrefixTableEntry
    updatePrefixTableEntry singletonRoute routes = let newRoute = slHead singletonRoute
                                                       pIsNotOldRoute r = peerData r /= peerData newRoute
                                                   in SL.insert newRoute $ SL.filter pIsNotOldRoute routes

    newSingletonPrefixTableEntry = SL.singleton route
    (maybeOldPrefixTableEntry, newPrefixTable) = insertLookupWithKey f ipfx newSingletonPrefixTableEntry pt where
        f _ = updatePrefixTableEntry
    newPrefixTableEntry = maybe newSingletonPrefixTableEntry ( updatePrefixTableEntry newSingletonPrefixTableEntry ) maybeOldPrefixTableEntry
    newBestRoute = slHead newPrefixTableEntry
    isNewBestRoute = newBestRoute == route

withdrawPrefixTable :: PrefixTable -> IPrefix -> PeerData -> (PrefixTable,Bool)
withdrawPrefixTable pt (IPrefix ipfx) peer = (pt', wasBestRoute) where
-- TODO - make resilient against lookup failure which could happen if a peer withdrew routes it had not sent....
    (Just oldRouteList , pt') = updateLookupWithKey f ipfx pt
    f :: Int -> PrefixTableEntry -> Maybe PrefixTableEntry
    f _ routes = let routes' = SL.filter (notPeer peer) routes in
         if null routes' then Nothing else Just routes'
    notPeer :: PeerData -> RouteData -> Bool
    notPeer pd rd = pd /= peerData rd 
    oldBestRoute = slHead oldRouteList
    wasBestRoute = peerData oldBestRoute == peer

withdraw :: PrefixTable -> [IPrefix] -> PeerData -> (PrefixTable,[IPrefix])
withdraw rib prefixes peer = Data.List.foldl' f (rib,[]) prefixes where
    f (pt,withdrawn) pfx = if p then (pt',pfx:withdrawn) else (pt',withdrawn) where
        (pt',p) = withdrawPrefixTable pt pfx peer
