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

import Data.IntMap.Strict
import qualified Data.SortedList as SL -- package sorted-list
import qualified Data.List

import BGPData(RouteData)
import Prefixes (IPrefix(..))

type PrefixTableEntry = SL.SortedList RouteData 
type PrefixTable = IntMap PrefixTableEntry

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
