{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import qualified Data.List
import Data.IP
import Data.Word

import Common
import Prefixes
import BGPReader(readRib,Rib,readGroupedRib)
import qualified Overlap
import PathAttributes
import PrefixTable
import BGPData
import PrefixTableUtils
import AdjRIBOut

main = do

    rib <- readGroupedRib
    putStrLn $ "routes: " ++ show (length rib)
    let prefixCount = sum (map (length .snd) rib)
    putStrLn $ "prefixes: " ++ show prefixCount
    -- putStrLn $ unlines $ mapt (customShowRoute . snd , shorten ) rib
    -- putStrLn $ unlines $ map (customShowRoute . snd . fst ) rib
    let paths = map (stripASPath . getASPath . snd . fst) rib
        simplePaths = map flattenPath paths
        longestPath = Data.List.maximum (map length simplePaths)
        simplerPaths = map removePrepends simplePaths
        longestPathWithoutPrepending = Data.List.maximum (map length simplerPaths)
        uniqueAScount = length $ Data.List.nub $ concat simplePaths
        endAScount = length $ Data.List.nub $ map last simplePaths
        transitASes = concatMap (tail.reverse) simplerPaths 
        transitAScount = length $ Data.List.nub transitASes
        transitASDistribution = distribution_ 10 transitASes
        transitASDistribution' = distribution transitASes
    putStrLn   "\nAS analysis"
    putStrLn $ "longestPath:  " ++ show longestPath
    putStrLn $ "longestPathWithoutPrepending:  " ++ show longestPathWithoutPrepending
    putStrLn $ "uniqueAScount:  " ++ show uniqueAScount
    putStrLn $ "endAScount:     " ++ show endAScount
    putStrLn $ "transitAScount: " ++ show transitAScount
    putStrLn $ "transitAS distribution:\n" ++ unlines ( map show transitASDistribution )
    putStrLn $ "transitAS distribution':\n" ++ unlines ( map show transitASDistribution' )
    putStrLn $ reportSegments paths

customShowRoute = showASPath . getASPath
-- customShowRoute route = show (pathAttributes route)

mapt (f,g) = map (\(a,b) -> (f a ++ " " ++ g b))
showASPath = showPath . stripASPath

stripASPath :: PathAttribute -> [ASSegment Word32]
stripASPath (PathAttributeASPath (ASPath2 path)) = stripASPath $ PathAttributeASPath (toASPath4 (ASPath2 path))
stripASPath (PathAttributeASPath (ASPath4 path)) = path
stripASPath (PathAttributeAS4Path (ASPath4 path)) = path
stripASPath (PathAttributeAS4Path (ASPath2 path)) = undefined

reportSegments paths = unlines [heading,all,sequences,sequenceSet1,sequenceSetN,seqSetSeq] where
    heading = "\nSequence Analysis"
    all = "all " ++ show (length paths)
    sequences = "sequences " ++ show ( length $ filter matchSeq paths)
    sequenceSet1 = "sequenceSet1 " ++ show ( length $ filter matchSeqSet1 paths)
    sequenceSetN = "sequenceSetN " ++ show ( length $ filter matchSeqSet paths)
    seqSetSeq = "seqSetSeq " ++ show ( length $ filter matchSeqSetSeq paths)

matchSeq [ASSequence _] = True
matchSeq _ = False

matchSeqSet1 [ASSequence _ , ASSet [_]] = True
matchSeqSet1 _ = False

matchSeqSet [ASSequence _ , ASSet [_]] = False
matchSeqSet [ASSequence _ , ASSet _] = True
matchSeqSet _ = False

matchSeqSetSeq [ASSequence _ , ASSet _, ASSequence _] = True
matchSeqSetSeq _ = False

flattenPath :: [ASSegment Word32] -> [Word32]
--flattenPath _ = []
flattenPath [] = []
flattenPath (ASSequence []:segs) = flattenPath segs
flattenPath (ASSequence asns:segs) = asns ++ flattenPath segs
flattenPath (ASSet []:segs) = flattenPath segs
flattenPath (ASSet asns:segs) = head asns : flattenPath segs

removePrepends :: [Word32] -> [Word32]
--removePrepends _ = []
removePrepends [] = []
removePrepends [x] = [x]
removePrepends (x:y:ax) | x==y = removePrepends (y:ax)
                        | otherwise = x : removePrepends (y:ax)


showPath [ASSequence seq1 , ASSet set, ASSequence seq2] = "SEQ+SET+SEQ " ++ show seq1 ++ " / " ++ show set ++ " / " ++ show seq2
showPath [ASSequence seq , ASSet set] = "SEQ+SET     " ++ show seq ++ " / " ++ show set
showPath [ASSequence seq] = "SEQ-       " ++ show seq
showPath [] = "EMPTY       "
showPath x = "UNKNOWN     " ++ show x
