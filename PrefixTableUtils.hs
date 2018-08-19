module PrefixTableUtils where

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
import qualified Data.Tuple as Data.Tuple
import Data.IP

import Common
import BGPData
import Prefixes (IPrefix(..))
import PrefixTable(PrefixTable,slHead)

-- ===================================================
--
-- some useful functions on prefix tables:
--
-- ===================================================

getRIB :: PrefixTable -> [(RouteData,IPrefix)]
getRIB pt = map f (toList pt) where
    f (pfx,routes) = (slHead routes , IPrefix pfx)

getFIB :: PrefixTable -> [(IPrefix,IPv4)]
getFIB pt = map f (getRIB pt) where
    f (route,pfx) = (pfx , nextHop route)

getAdjRIBOut :: PrefixTable -> [(RouteData,[IPrefix])]
-- getAdjRIBOut _ = []
getAdjRIBOut = groupBy_ . getRIB
    -- f (routes,pfxs) = (slHead routes, map IPrefix pfxs)


showPrefixTable :: PrefixTable -> String
showPrefixTable pt = unlines $ map showPrefixTableItem (toList pt) where
    -- this below version show the next hop only - this should be in the route itself but for now uses the source peers IPv4 as the next hop....
    showPrefixTableItem (k,v) = unlines $ map (\route -> show (IPrefix k) ++ " " ++ ( show.peerIPv4.peerData) route ++ " (" ++ (show.pathLength) route ++ ")" ) (SL.fromSortedList v)

    -- this below version show the 'whole' linked route
    -- showPrefixTableItem (k,v) = unlines $ map (\route -> show (IPrefix k) ++ " " ++ show route) (SL.fromSortedList v)
{-
groupedByRoutePrefixes :: PrefixTable -> [(PrefixTableEntry,[Int])]
groupedByRoutePrefixes = groupBy_ . ( map Data.Tuple.swap ) . toList

-- groupedByRoutePrefixes' :: PrefixTable -> [[(Int,PrefixTableEntry)]]
-- groupedByRoutePrefixes' pt = Data.List.groupBy sameRoute prefixes where
--     prefixes = Data.List.sortOn (fst) $ map Data.Tuple.swap $ toList pt
--     sameRoute (a,_) (b,_) = a == b

showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute pt = unlines $ map showRoute (groupedByRoutePrefixes pt) where
    showRoute (r,pfxs) = unlines $ ( show r ) :
                       showBest (head pfxs) :
                       map showOther ( tail pfxs)
                       where
                           showBest pfx = "*  " ++ show' pfx
                           showOther pfx = "   " ++ show' pfx
                           show'  = show . IPrefix

-}
showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute _ = ""
