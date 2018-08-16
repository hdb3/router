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

import BGPData(RouteData,PeerData,peerData,peerIPv4,pathLength)
import Prefixes (IPrefix(..))

type PrefixTableEntry = SL.SortedList RouteData 
type PrefixTable = IntMap PrefixTableEntry

newPrefixTable :: PrefixTable
newPrefixTable = Data.IntMap.Strict.empty

update:: PrefixTable -> [IPrefix] -> RouteData -> (PrefixTable,[IPrefix])
update pt pfxs route = Data.List.foldl' f (pt,[]) pfxs where
    f (pt_,updated) pfx = if p then (pt__,pfx:updated) else (pt__,updated) where
        (pt__,p) = updatePrefixTable pt_ pfx route

updatePrefixTable :: PrefixTable -> IPrefix -> RouteData -> (PrefixTable,Bool)
updatePrefixTable pt (IPrefix ipfx) route = (newPrefixTable, isNewBestRoute) where 
    head sl = x where
        Just (x,_) = SL.uncons sl
    updatePrefixTableEntry :: PrefixTableEntry -> PrefixTableEntry -> PrefixTableEntry
    updatePrefixTableEntry routes singletonRoute = SL.insert (head singletonRoute) routes
    newSingletonPrefixTableEntry = SL.singleton route
    f key new_value old_value = f' new_value old_value
    f' new_value old_value = updatePrefixTableEntry new_value old_value
    (maybeOldPrefixTableEntry, newPrefixTable) = insertLookupWithKey f ipfx newSingletonPrefixTableEntry pt
    newPrefixTableEntry = maybe newSingletonPrefixTableEntry ( f' newSingletonPrefixTableEntry ) maybeOldPrefixTableEntry
    newBestRoute = head newPrefixTableEntry
    isNewBestRoute = newBestRoute == route

showPrefixTable :: PrefixTable -> String
showPrefixTable pt = unlines $ map showPrefixTableItem (toList pt) where
    -- this below version show the next hop only - this should be in the route itself but for now uses the source peers IPv4 as the next hop....
    showPrefixTableItem (k,v) = unlines $ map (\route -> show (IPrefix k) ++ " " ++ ( show.peerIPv4.peerData) route ++ " (" ++ (show.pathLength) route ++ ")" ) (SL.fromSortedList v)

    -- this below version show the 'whole' linked route
    -- showPrefixTableItem (k,v) = unlines $ map (\route -> show (IPrefix k) ++ " " ++ show route) (SL.fromSortedList v)

showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute pt = unlines $ map showRoute groupedByRoutePrefixes where
    prefixes = Data.List.sortOn (snd) $ toList pt
    groupedByRoutePrefixes = Data.List.groupBy sameRoute prefixes
    sameRoute (_,a) (_,b) = a == b
    showRoute groups = unlines $ ( show $ snd $ head groups ) : -- this is the route, same for all of the follwoing prefixes
                       showBest (head groups) :
                       map showOther ( tail groups)
                       where
                           showBest pfx = "*  " ++ show' pfx
                           showOther pfx = "   " ++ show' pfx
                           show'  = show . IPrefix . fst

-- ###########################################################


withdrawPrefixTable :: PrefixTable -> IPrefix -> PeerData -> (PrefixTable,Bool)
withdrawPrefixTable pt (IPrefix ipfx) peer = (pt', wasBestRoute) where
    (Just oldRouteList , pt') = updateLookupWithKey f ipfx pt
    f :: Int -> PrefixTableEntry -> Maybe PrefixTableEntry
    f _ routes = let routes' = SL.filter (notPeer peer) routes in
         if null routes' then Nothing else Just routes'
    notPeer :: PeerData -> RouteData -> Bool
    notPeer pd rd = pd /= ( peerData rd ) 
    head sl = x where
        Just (x,_) = SL.uncons sl
    oldBestRoute = head oldRouteList
    wasBestRoute = (peerData oldBestRoute) == peer

withdraw :: PrefixTable -> [IPrefix] -> PeerData -> (PrefixTable,[IPrefix])
withdraw rib prefixes peer = Data.List.foldl' f (rib,[]) prefixes where
    f (pt,withdrawn) pfx = if p then (pt',pfx:withdrawn) else (pt',withdrawn) where
        (pt',p) = withdrawPrefixTable pt pfx peer
