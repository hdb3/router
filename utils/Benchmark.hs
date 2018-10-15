module Main where
-- import Data.Time.Clock as DT
import qualified Data.Time.Clock.System as DT

import BGPReader(readRib)

main = do
    -- t0 <- DT.getCurrentTime
    t0 <- DT.getSystemTime
    rib <- readRib
    -- t1 <- DT.getCurrentTime
    t1 <- DT.getSystemTime
    stopwatch "loaded rib" t0
    --putStrLn $ "t0: " ++ show t0
    --putStrLn $ "t1: " ++ show t1
    putStrLn $ "loaded rib in " ++ show (diffSystemTime t0 t1)
    --putStrLn $ "loaded rib in " ++ show (diffSystemTime t1 t0)
    -- putStrLn $ "loaded rib in " ++ show (diffUTCTime t1 t0)
    --putStrLn $ "loaded rib in " ++ show (ST.normalizeTimeDiff (ST.diffClockTimes t1 t0))
    --putStrLn $ "loaded rib in " ++ (ST.timeDiffToString (ST.diffClockTimes t1 t0))
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    print (last rib)
    stopwatch "printed from rib" t0

diffSystemTime (DT.MkSystemTime s0 ns0) (DT.MkSystemTime s1 ns1) = 
    f s1 ns1 - f s0 ns0 where
    f s ns = ( 0.0 + fromIntegral (s * 1000000000) + fromIntegral ns ) / 1000000000.0

stopwatch s t = do
    t' <- DT.getSystemTime
    putStrLn $ s ++ " " ++ show (diffSystemTime t t')


