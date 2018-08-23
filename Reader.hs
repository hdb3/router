module Main where

import System.Environment
import System.IO
{-
import qualified Data.ByteString.Lazy as L
import Data.Binary(Binary(..))
import Data.Binary.Get(runGet)
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust)
import Data.Word
import Data.Bits

import Common
import BGPparse
import GetBGPMsg
import Update
import PathAttributes
import Prefixes
import NewRib
import BGPData
import PrefixTable
--import PrefixTableUtils
import Report
-}
import BGPReader

main = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then
         hPutStrLn stderr "no filename specified"
    else do
        rib <- bgpReader (args !! 0)
        putStrLn $ "got " ++ show (length rib) ++ "prefixes"
