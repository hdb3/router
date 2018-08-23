module Main where

import System.Environment
import System.IO

import BGPReader

main = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then
         hPutStrLn stderr "no filename specified"
    else do
        rib <- bgpReader (args !! 0)
        putStrLn $ "got " ++ show (length rib) ++ " prefixes"
        print (last rib)
