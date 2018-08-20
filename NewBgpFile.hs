{-# LANGUAGE RecordWildCards #-}
module Main where
import System.Environment
import System.IO
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

verbose = False

main = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if (null args) then
         hPutStrLn stderr $ "no filename specified"
    else do
        handle <- openBinaryFile (args !! 0) ReadMode
        stream <- L.hGetContents handle
        let msgs = runGet getBGPByteStrings stream
            parsedMsgs = map decodeBGPByteString msgs
            updateMsgs = (map getUpdateP) $ getUpdatesFrom parsedMsgs
            limit = if n > 0 then take n else id
        processUpdates ( limit updateMsgs )

processUpdates updates = do
        hPutStrLn stderr $ "read " ++ show (length updates) ++ " updates"
        rib <- newRib
        mapM_ (updateRib rib) updates
        rib' <- getRib rib
        report rib'

analyse BGPUpdateP{..} = do
   hPutStrLn stderr $ (show $ length nlriP) ++ " prefixes " ++ (show $ length withdrawnP) ++ " withdrawn " ++ (show $ getASPathLength attributesP) ++ " = pathlength "
list  :: (Show t) => [t] -> String
list = unlines . (map show)

csum :: L.ByteString -> Word8
csum = L.foldl' xor 0

getUpdatesFrom msgs = foldr keepOnlyUpdates [] msgs where
                          keepOnlyUpdates a@(BGPUpdate{}) ax = a:ax
                          keepOnlyUpdates _ ax = ax

dropUpdatesFrom msgs = foldr keepOnlyUpdates [] msgs where
                          keepOnlyUpdates a@(BGPUpdate{}) ax = ax
                          keepOnlyUpdates a ax = a:ax

updateRib rib BGPUpdateP{..} = do
                ribUpdateMany rib attributesP hashP nlriP
                ribWithdrawMany rib withdrawnP
