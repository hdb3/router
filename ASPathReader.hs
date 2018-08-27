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
    putStrLn $ unlines $ map (customShowRoute . snd . fst ) rib

customShowRoute = showASPath . getASPath
-- customShowRoute route = show (pathAttributes route)

showASPath = showPath . stripASPath -- where
stripASPath :: PathAttribute -> [ASSegment Word32]
stripASPath (PathAttributeASPath (ASPath2 path)) = stripASPath $ PathAttributeASPath (toASPath4 (ASPath2 path))
stripASPath (PathAttributeASPath (ASPath4 path)) = path
stripASPath (PathAttributeAS4Path (ASPath4 path)) = path
stripASPath (PathAttributeAS4Path (ASPath2 path)) = undefined

showPath [ASSequence seq1 , ASSet set, ASSequence seq2] = "SEQ+SET+SEQ " ++ show seq1 ++ " / " ++ show set ++ " / " ++ show seq2
showPath [ASSequence seq , ASSet set] = "SEQ+SET     " ++ show seq ++ " / " ++ show set
showPath [ASSequence seq] = "SEQ-       " ++ show seq
showPath [] = "EMPTY       "
showPath x = "UNKNOWN     " ++ show x
