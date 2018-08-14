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

main = withdrawTest

updateTest = do
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
   putStrLn $ showPrefixTable rib
   putStrLn $ showPrefixTableByRoute rib
   let (resPT,resPFX) = PrefixTable.update newPrefixTable prefixList1 route11
   print resPFX
   putStrLn $ showPrefixTableByRoute resPT

withdrawTest = do
   let l1 = ["1.2.3.4/32" ,"1.2.3.4/24" ,"1.2.3.4/16" ,"1.2.3.4/8"]
       l1_1 = ["1.2.3.4/32" ]
       (pt,_) = PrefixTable.update newPrefixTable l1 gd1Peer1Route1
   putStrLn $ showPrefixTableByRoute pt

   putStrLn "\nWithdraw\n"

   let (pt',pfxs) = withdraw pt l1_1 gd1Peer1
   print pfxs
   putStrLn $ showPrefixTableByRoute pt'
