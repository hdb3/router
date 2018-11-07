{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Maybe(isJust)
import qualified Data.Time.Clock.System as DT
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Text.Printf

import BGPlib
import BGPReader(updateRib,readRib,readGroupedRib)
import qualified BGPRib
import RIBData
import RibDef
import MapRib
import qualified IP4Prefix

t = DT.getSystemTime
peer1 = Peer "peer1" True  64501 "10.0.0.1" "10.0.0.1" "10.0.0.99"
peer2 = Peer "peer2" False 64502 "10.0.0.2" "10.0.0.2" "10.0.0.99"
peer3 = Peer "peer3" False 64503 "10.0.0.3" "10.0.0.3" "10.0.0.99"

emptyMapRib = mkRib compare :: MapRib
emptyMapRib' = ([], emptyMapRib)

parseRibRoute ((_,attributes),prefix) = (IP4Prefix.fromAddrRange $ toAddrRange prefix, RIBData.makeRoute True attributes)

getRoutes = do
    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    return $ map parseRibRoute rib

--query :: ([(RibDef.Prefix, (Peer, Route))], MapRib) -> IO ()
query rib = do let r = RibDef.lookup "255.255.255.255" rib
               putStrLn $ if isJust r then "query complete!" else "query complete!" 


--main = test3
main = do
    t0 <- t
    routes <- getRoutes
    let peerRoutes peer routes = map (\(pfx,rte) -> (pfx, routePrePendAS (peerAS peer) rte)) routes where
            routePrePendAS p r = r { pathAttributes = prePendAS p (pathAttributes r) }
        peer1Routes = peerRoutes peer1 routes
        peer2Routes = peerRoutes peer2 routes
        peer3Routes = peerRoutes peer3 routes
    stopwatch "loaded rib" t0
    let mapRib1 = buildUpdateSequence peer2 peer2Routes emptyMapRib
    query mapRib1
    stopwatch "populated mapRib with peer 2" t0

    let mapRib2 = buildUpdateSequence peer1 peer1Routes mapRib1
    query mapRib2
    stopwatch "populated mapRib with peer 1" t0

    let mapRib3 = buildUpdateSequence peer3 peer3Routes mapRib2
    query mapRib3
    stopwatch "populated mapRib with peer 3" t0

    let mapRib4 = buildUpdateSequence peer2 peer2Routes mapRib3
    query mapRib4
    stopwatch "repopulated mapRib with peer 2" t0


{-


    let mapRib3 = removePeer_ peer2 mapRib2
    query mapRib3
    stopwatch "depopulated mapRib with pref peer" t0

    let mapRib2 = buildUpdateSequence peer3 (take 10 routes) mapRib
        mapRib2' = buildUpdateSequence' peer3 (take 10 routes) mapRib'
    query mapRib2
    stopwatch "populated mapRib with non-pref peer" t0
    let mapRib3 = removePeer_ peer3 mapRib2
    query mapRib3
    stopwatch "depopulated mapRib with non-pref peer" t0

-}


test1 = do
    putStrLn "test1 - read file with BGPReader(readRib)"
    t0 <- DT.getSystemTime
    rib <- readRib
    let routes = map parseRibRoute rib
    t1 <- DT.getSystemTime
    stopwatch "loaded rib" t0
    putStrLn $ "loaded rib in " ++ show (diffSystemTime t0 t1)
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    print (last rib)
    stopwatch "printed from rib" t0

diffSystemTime :: DT.SystemTime -> DT.SystemTime -> Double
diffSystemTime (DT.MkSystemTime s0 ns0) (DT.MkSystemTime s1 ns1) = 
    f s1 ns1 - f s0 ns0 where
    f s ns = ( 0.0 + fromIntegral (s * 1000000000) + fromIntegral ns ) / 1000000000.0

stopwatch s t = do
    t' <- DT.getSystemTime
    -- putStrLn $ s ++ " " ++ show (diffSystemTime t t')
    let dT = diffSystemTime t t'
    putStrLn $ s ++ " " ++ if 1.0 > dT then printf "%.3f mS" (1000*dT) else printf "%.3f S" dT 

timer s f = do
    putStrLn $ "timing function " ++ s
    t0 <- DT.getSystemTime
    f
    stopwatch (s ++ "completed in ") t0 

test2 = do
    t0 <- DT.getSystemTime
    contents <- L.getContents
    putStrLn $ "file length: " ++ show (L.length contents) ++ " bytes"
    stopwatch "after file read" t0 
    let bgpByteStrings = runGet getBGPByteStrings contents
    putStrLn $ "BGP message count : " ++ show (length bgpByteStrings)
    stopwatch "after wireformat parse" t0 
    let
        bgpMessages = map decodeBGPByteString bgpByteStrings
        updates = map BGPRib.getUpdate $ filter isUpdate bgpMessages
    rib <- BGPRib.newRib BGPRib.dummyPeerData
    mapM_ (updateRib rib) updates
    stopwatch "after full (?) parse into rib" t0 

test3 = do
    t0 <- DT.getSystemTime
    rib <- readRib
    putStrLn $ "rib length: " ++ show (length rib)
    stopwatch "rib read duration " t0 
    t1 <- DT.getSystemTime
    grib <- readGroupedRib
    putStrLn $ "grib length: " ++ show (length grib)
    stopwatch "grib read duration " t1 

testN = do
    t0 <- DT.getSystemTime
    rib <- readRib
    putStrLn $ "rib length: " ++ show (length rib)
    stopwatch "after rib read" t0 
