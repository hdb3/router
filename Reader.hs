module Main where
import System.IO

import BGPReader(readRib)

main = do
    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    print (last rib)
