module Main where
-- import Data.Time.Clock as DT
import qualified Data.Time.Clock.System as DT
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Text.Printf

import BGPlib
import BGPReader(updateRib,readRib,readGroupedRib)
import qualified BGPRib

main = test3

test1 = do
    putStrLn "test1 - read file with BGPReader(readRib)"
    t0 <- DT.getSystemTime
    rib <- readRib
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
