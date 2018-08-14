{-#LANGUAGE OverloadedStrings #-}
module Main where

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
import PrefixTable
import BGPDataTestData
{- 
type PrefixTableEntry = SL.SortedList RouteData 
type PrefixTable = IntMap PrefixTableEntry

newPrefixTable = Data.IntMap.Strict.empty

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
-}

prefixList1 = 
        ["1.2.3.4/32"
        ,"1.2.3.4/24"
        ,"1.2.3.4/16"
        ,"1.2.3.4/8"
        , "0.0.0.0/0"
        , "192.168.1.99/24"
        , "129.129.0.0/16"
        , "172.16.0.77/12"
        , "169.254.108.17/32"
        , "10.1.2.3/8"
        ] :: [IPrefix]

main = do
   putStrLn "PrefixTable-test"
   let pt = newPrefixTable
       update_ pfx rte t = fst $ updatePrefixTable t pfx rte
       rib =   ( update_ "192.168.1.0/24" route11 )
             $ ( update_ "192.168.2.0/24" route11 )
             $ ( update_ "192.168.3.0/24" route11 )
             $ ( update_ "192.168.11.0/24" route12 )
             $ ( update_ "192.168.12.0/24" route12 )
             $ ( update_ "192.168.13.0/24" route12 )
             $ newPrefixTable
       -- (pt',p') = updatePrefixTable pt "192.168.1.99/24" route11
       -- (pt'',p'') = updatePrefixTable pt' "192.168.1.77/24" route12
       -- (pt'',p'') = updatePrefixTable pt' "192.168.1.77/24" route12
   -- print pt'
   -- print pt''
   -- putStrLn $ showPrefixTable pt''
   putStrLn $ showPrefixTable rib
   putStrLn $ showPrefixTableByRoute rib
   let (resPT,resPFX) = PrefixTable.update newPrefixTable prefixList1 route11
   print resPFX
   putStrLn $ showPrefixTableByRoute resPT

