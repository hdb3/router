{-# LANGUAGE RecordWildCards #-}
module Main where
import System.Environment
import System.IO(Handle,openBinaryFile,IOMode(..))
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
import Rib

verbose = False

main = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if (null args) then
         putStrLn "no filename specified"
    else do
        handle <- openBinaryFile (args !! 0) ReadMode
        stream <- L.hGetContents handle
        let msgs = runGet getBGPByteStrings stream
            parsedMsgs = map decodeBGPByteString msgs
            updateMsgs = (map getUpdateP) $ getUpdatesFrom parsedMsgs
            limit = if n > 0 then take n else id
        processUpdates ( limit updateMsgs )

processUpdates updates = do
        rib <- newRib2
        -- putStrLn $ list updates
        mapM analyse updates
        -- mapM_ (updateRib rib) updates
        summary rib >>= putStrLn

analyse BGPUpdateP{..} = do
   putStrLn $ (show $ length nlriP) ++ " prefixes " ++ (show $ length withdrawnP) ++ " withdrawn " ++ (show $ getASPAthLength attributesP) ++ " = pathlength "
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
                ribUpdateMany rib (attributesP,fromRaw' rawAttributes) nlriP
                ribWithdrawMany rib withdrawnP