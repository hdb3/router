{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.IP
import RibDef
import RIBData
import IP4Prefix

main = do
    putStrLn "RibTest"

    let riby = buildPeerSequence peer1 routes emptyRib
        ribz = buildPeerSequence peer2 routes riby

    let riby' = buildPeerSequence' peer1 routes emptyRib'
        ribz' = buildPeerSequence' peer2 routes ([],riby)
        ribz'' = buildPeerSequence' peer2 routes $ buildPeerSequence' peer1 routes emptyRib'
        ribz''' = buildPeerSequence' peer1 routes $ buildPeerSequence' peer2 routes emptyRib'

    putStrLn "\n---------------\n"
    putStrLn "riby\n"
    print riby
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ concat $ fst riby'

    putStrLn "\n---------------\n"
    putStrLn "ribz\n"
    print ribz
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ concat $ fst ribz'
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ concat $ fst ribz''
    putStrLn "\n---------------\n"

    putStrLn "\n---------------\n"
    putStrLn "ribz'''\n"
    print $ snd ribz'''
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ concat $ fst ribz'''
    putStrLn "\n---------------\n"

showRibChanges = unlines . map showRibChange . reverse
showRibChange (prefix, Nothing, Nothing) = "nul " ++ show prefix
showRibChange (prefix, Nothing, Just a) = "Add " ++ show prefix ++ " via " ++ (peerName $ fst a)
showRibChange (prefix, Just a, Just b) = "Chg " ++ show prefix ++ " via " ++ (peerName $ fst a) ++ " -> " ++ (peerName $ fst b)
showRibChange (prefix, Just a, Nothing) = "Del " ++ show prefix ++ " via " ++ (peerName $ fst a)

emptyRib = mkRib compare :: MapRib
emptyRib' = ([], emptyRib)
makeUpdateAction peer (prefix,route) = update_ prefix peer route
buildPeerSequence peer routes = Main.sequence $ map (makeUpdateAction peer) routes
buildPeerSequence' peer routes = Main.sequence' $ map (makeUpdateAction peer) routes

sequence fx r = foldl (\b f -> snd $ f b) r fx

--sequence' fx r0 = foldl m ([],r0) fx where
sequence' fx (ax0,r0) = foldl m (ax0,r0) fx where
    m (ax,r) f = let (a,r') = f r 
                in (a:ax,r')
        
compose' f g r = let (cf,r') = f r
                     (cg,r'') = g r'
                 in ( cf ++ cg, r'')

compose f g = snd . g . snd . f

routes = [(prefix1,route1),(prefix2,route2),(prefix3,route3)]
peer1 = Peer "peer1" True 64500 "10.0.0.1" "10.0.0.1" "10.0.0.99"
peer2 = Peer "peer2" False 64501 "10.0.0.2" "10.0.0.2" "10.0.0.99"
route1 = Route "route1" 100 [] 5 0 0 True
route2 = Route "route2" 200 [] 4 1 0 False
route3 = Route "route3" 300 [] 3 1 0 False
prefix1 = "192.168.1.0/24" :: IP4Prefix
prefix2 = "192.168.2.0/24" :: IP4Prefix
prefix3 = "192.168.3.0/24" :: IP4Prefix
