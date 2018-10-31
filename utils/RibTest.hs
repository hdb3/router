{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.IP
import RibDef
import RIBData
import IP4Prefix

main = do
    putStrLn "RibTest"
    let emptyRib = mkRib compare :: MapRib
        routes = [(prefix1,route1),(prefix2,route2),(prefix3,route3)]
        actions = Main.sequence [ update_ prefix1 peer1 route1
                                , update_ prefix2 peer2 route2
                                , update_ prefix2 peer1 route1
                                , withdraw_ prefix1 peer1
                                , removePeer_ peer2
                                ]
        makeUpdateAction peer (prefix,route) = update_ prefix peer route
        --actions2 = Main.sequence $ map (makeUpdateAction peer2) routes
        buildPeerSequence peer routes = Main.sequence $ map (makeUpdateAction peer) routes
        actions1 = buildPeerSequence peer1 routes
        actions2 = buildPeerSequence peer2 routes
        ribx = actions emptyRib
        riby = actions1 emptyRib
        ribz = actions2 riby
    putStrLn "\n---------------\n"
    putStrLn "ribx\n"
    print ribx
    putStrLn "\n. . . . . . . .\n"

    putStrLn "\n---------------\n"
    putStrLn "riby\n"
    print riby
    putStrLn "\n. . . . . . . .\n"

    putStrLn "\n---------------\n"
    putStrLn "ribz\n"
    print ribz
    putStrLn "\n. . . . . . . .\n"

sequence fx r = foldl (\b f -> snd $ f b) r fx
sequence' fx r0 = foldl m ([],r0) fx where
    m (ax,r) f = let (a,r') = f r 
                in (a:ax,r')
        
compose' f g r = let (cf,r') = f r
                     (cg,r'') = g r'
                 in ( cf ++ cg, r'')

compose f g = snd . g . snd . f

peer1 = Peer "peer1" True 64500 "10.0.0.1" "10.0.0.1" "10.0.0.99"
peer2 = Peer "peer2" False 64501 "10.0.0.2" "10.0.0.2" "10.0.0.99"
route1 = Route "route1" 100 [] 5 0 0 True
route2 = Route "route2" 200 [] 4 1 0 False
route3 = Route "route3" 300 [] 3 1 0 False
prefix1 = "192.168.1.0/24" :: IP4Prefix
prefix2 = "192.168.2.0/24" :: IP4Prefix
prefix3 = "192.168.3.0/24" :: IP4Prefix
