{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.IP
import RibDef
import RIBData
import IP4Prefix

main = do
    putStrLn "RibTest"
    let rib0 = mkRib compare :: MapRib
    -- let rib = mkRib (compare :: ((Peer,Route) -> (Peer,Route) -> Ordering))
    print rib0
    print prefix1
    let rib1 = fst $ update rib0 prefix1 peer1 route1
        rib2 = fst $ update rib1 prefix2 peer2 route2
        rib3 = fst $ update rib2 prefix2 peer1 route1
    putStrLn "\n---------------\n"
    print rib3
    putStrLn "\n. . . . . . . .\n"
    print $ dumpRib rib3
    putStrLn "\n---------------\n"
{-
    print rib2
    print $ RibDef.lookup rib2 prefix1
    print $ RibDef.lookup rib2 prefix2
-}

peer1 = Peer "peer1" True 64500 "10.0.0.1" "10.0.0.1" "10.0.0.99"
peer2 = Peer "peer2" False 64501 "10.0.0.2" "10.0.0.2" "10.0.0.99"
route1 = Route "route1" 100 [] 5 0 0 True
route2 = Route "route2" 200 [] 4 1 0 False
prefix1 = "192.168.1.0/24" :: IP4Prefix
prefix2 = "192.168.2.0/24" :: IP4Prefix
prefix3 = "192.168.3.0/24" :: IP4Prefix
