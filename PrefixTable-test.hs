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
import Control.Exception(assert)

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

l0 = ["1.2.3.4/32"] :: [IPrefix]
l1 = ["1.2.3.4/32" ,"1.2.3.4/24" ,"1.2.3.4/16" ,"1.2.3.4/8"] :: [IPrefix]
l2 = ["2.2.3.4/32" ,"2.2.3.4/24" ,"2.2.3.4/16" ,"2.2.3.4/8"] :: [IPrefix]
l1_1 = ["1.2.3.4/32" ] :: [IPrefix]
l1_2_4 = ["1.2.3.4/24" ,"1.2.3.4/16" ,"1.2.3.4/8"] :: [IPrefix]

update_ pfxs rte t = fst $ PrefixTable.update t pfxs rte
withdraw_ pfxs peer t = fst $ PrefixTable.withdraw t pfxs peer

main = do
    -- selectTest1
    -- selectTest2
    selectTestM

showRib = showPrefixTable
-- showRib = showPrefixTableByRoute

updateTest = do
   putStrLn "updateTest"
   let pt = newPrefixTable
       rib =   ( update_ ["192.168.1.0/24"] route11 )
             $ ( update_ ["192.168.2.0/24"] route11 )
             $ ( update_ ["192.168.3.0/24"] route11 )
             $ ( update_ ["192.168.11.0/24"] route12 )
             $ ( update_ ["192.168.12.0/24"] route12 )
             $ ( update_ ["192.168.13.0/24"] route12 )
             $ newPrefixTable
   putStrLn $ showPrefixTable rib
   putStrLn $ showRib rib
   let (resPT,resPFX) = PrefixTable.update newPrefixTable prefixList1 route11
   print resPFX
   putStrLn $ showRib resPT

withdrawTest = do
   putStrLn "\nWithdraw test\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l1 gd1Peer1Route1
       (pt1,_) = PrefixTable.update pt0 l2 gd1Peer1Route2
   tell' "pt1" pt1

   let pt2 = fst $ withdraw pt1 l1_1 gd1Peer1
   tell' "pt2" pt2

   let pt3 = fst $ withdraw pt2 l1_2_4 gd1Peer1
   tell' "pt3" pt3

   let pt4 = fst $ withdraw pt3 l2 gd1Peer1
   tell' "pt4" pt4

selectTest1 = do
   putStrLn "\nselectTest1\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l0 gd1Peer1Route1
       (pt1,_) = PrefixTable.update pt0 l0 gd1Peer1Route2
   tell' "pt0" pt0
   assert (pt0 == pt1) $ tell' "pt1" pt1

selectTest2 = do
   putStrLn "\nselectTest2\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l0 gd1Peer1Route2
       (pt1,_) = PrefixTable.update newPrefixTable l0 gd1Peer1Route1
       (pt2,_) = PrefixTable.update newPrefixTable l0 gd1Peer1Route1
   tell' "pt0" pt0
   tell' "pt1" pt1
   tell' "pt2" pt2
   assert (pt1 == pt2) $ putStrLn "OK"

selectTestM = do
   putStrLn "\nselectTestM\n"
   let
       pt0 = update_ l1 gd1Peer1Route2 $ update_ l1 gd1Peer2Route2 newPrefixTable
       pt1 = update_ l1 gd1Peer1Route1 pt0
       -- (pt0,_) = PrefixTable.update newPrefixTable l1     gd1Peer1Route2
       -- (pt1,_) = PrefixTable.update pt0            l1     gd1Peer2Route2
       -- (pt2,_) = PrefixTable.update pt1            l1_2_4 gd1Peer1Route1
       -- (pt3,_) = PrefixTable.update pt2            l1_2_4 gd1Peer2Route1
       -- (pt4,_) = PrefixTable.withdraw pt3          l1_1   gd1Peer1
       -- (pt5,_) = PrefixTable.withdraw pt4          l1     gd1Peer2
       -- (pt6,_) = PrefixTable.withdraw pt5          l1     gd1Peer1
   tell' "pt0" pt0
   tell' "pt1" pt1
   -- tell' "pt2" pt2
   -- tell' "pt3" pt3
   -- tell' "pt4" pt4
   -- tell' "pt5" pt5
   -- tell' "pt6" pt6

selectTestN = do
   putStrLn "\nselectTestN\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l1     gd1Peer1Route2
       (pt1,_) = PrefixTable.update pt0            l1     gd1Peer2Route2
       (pt2,_) = PrefixTable.update pt1            l1_2_4 gd1Peer1Route1
       (pt3,_) = PrefixTable.update pt2            l1_2_4 gd1Peer2Route1
       (pt4,_) = PrefixTable.withdraw pt3          l1_1   gd1Peer1
       (pt5,_) = PrefixTable.withdraw pt4          l1     gd1Peer2
       (pt6,_) = PrefixTable.withdraw pt5          l1     gd1Peer1
   tell' "pt0" pt0
   tell' "pt1" pt1
   tell' "pt2" pt2
   tell' "pt3" pt3
   tell' "pt4" pt4
   tell' "pt5" pt5
   tell' "pt6" pt6

tell' s pt = do
    putStrLn $ s ++ ": "
    putStrLn $ showRib pt
    putStrLn "==========================\n"

tell (pt,pfxs) = do
    putStr $ showRib pt
    putStr " -- "
    print pfxs
