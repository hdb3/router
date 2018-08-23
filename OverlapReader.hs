module Main where
import System.IO

import BGPReader(readRib)
import Overlap

main = do
    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    print (last rib)
    let allPrefixes = concatMap snd rib
    putStrLn $ "got " ++ show (length allPrefixes) ++ " prefixes"
    let t = Overlap.fromList allPrefixes
    putStrLn $ "tree contains " ++ show (size t)
