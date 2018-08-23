module Main where
import System.IO
import Data.List(sortOn)

import Prefixes
import BGPReader(readRib)
import Overlap

main = do
    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    -- print (last rib)
    let allPrefixes = concatMap snd rib
        -- drop default if present and also every thing longer than /24
        (real,fake) = span ((25 >) . l) $ dropWhile ((0 ==) . l) $ sortOn l allPrefixes
        l (Prefix (a,b)) = a
        
    putStrLn $ "got " ++ show (length real) ++ " prefixes"
    print $ ("real", take 5 real)
    print $ ("fake",fake)
{-
    let t = Overlap.fromList sortedAllPrefixes
    putStrLn $ "tree contains " ++ show (size t)
    putStrLn $ "tree height   " ++ show (height t)
    putStrLn $ "tree width    " ++ show (width t)
-}
