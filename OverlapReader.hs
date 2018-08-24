module Main where
import System.IO
import qualified Data.List

import Prefixes
import BGPReader(readRib)
import qualified Overlap2 as Overlap

main = do
    let sortPrefixes = Data.List.sortOn tuple where tuple (Prefix (a,b)) = (a,b)
        len (Prefix (a,b)) = a
        coverage (Prefix (a,_)) = 2^(32-a)
        fullCoverage = 2^32

    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    -- print (last rib)
    let allPrefixes = concatMap snd rib
        -- drop default if present and also every thing longer than /24
        (real,fake) = span ((25 >) . len) $ dropWhile ((0 ==) . len) $ sortPrefixes allPrefixes
        
    putStrLn $ "got " ++ show (length real) ++ " prefixes"

    -- these are useful sanity checks...
    -- print $ ("real", take 5 real)
    -- print $ ("fake",fake)

    let t = Overlap.fromList real
        r = Overlap.reduce t
        p = Overlap.partition r
        t' = sortPrefixes $ Overlap.toList r
        p' = sortPrefixes $ concatMap Overlap.toList p
        (single,multiple) = Data.List.partition ( (2 >) . Overlap.size) p

    if real /= t' then putStrLn "consistency check on Overlap.fromList / Overlap.toList FAILS!!!!" else return ()
    if real /= p' then putStrLn "consistency check on Overlap.partition FAILS!!!!" else return ()
    -- putStrLn $ "consistency check on Overlap.fromList / Overlap.toList " ++ if real == t' then "Success!" else "FAILS!!!!"

    putStrLn $ "tree contains " ++ show (Overlap.size t)
    putStrLn $ "tree height   " ++ show (Overlap.height t)
    -- putStrLn $ "longest   " ++ show (Overlap.longest t)
    putStrLn $ "tree contains " ++ show (length p) ++ " partitions"
    putStrLn $ show (length single) ++ " non-overlapping and " ++ show (length multiple) ++ " overlapping partitions"
    putStrLn $ "full distribution by size: " ++ show (distribution $ map Overlap.size p)
    putStrLn $ "full distribution by height: " ++ show (distribution $ map Overlap.height p)

    putStrLn "\nCoverage analysis\n"
    let heads = map Overlap.head p
        actualCoverage = sum $ map coverage heads
        overlapCoverage = sum $ map coverage real
    -- putStrLn $ "actual coverage is" ++ show $ sum $ map ( coverage . head ) p
    putStrLn $ "actual coverage is " ++ show actualCoverage ++ " (" ++ show (100 * actualCoverage `div` fullCoverage) ++ "%)"
    putStrLn $ "overlap coverage is " ++ show overlapCoverage ++ " (" ++ show (100 * overlapCoverage `div` fullCoverage) ++ "%)"



distribution :: [Int] -> [(Int,Int)]
distribution = map (\a -> (head a,length a)) . Data.List.group . Data.List.sort

