{-# LANGUAGE RecordWildCards #-}
module Main where
import System.Environment
import System.IO(Handle,openBinaryFile,IOMode(..))
import qualified Data.ByteString.Lazy as L
import Data.Binary(Binary(..))
import Data.Binary.Get(runGet)
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust)

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
        rib <- newRib2
        let msgs = runGet getBGPByteStrings stream
            parsedMsgs = map decodeBGPByteString msgs
            updateMsgs = (map getUpdateP) $ getUpdatesFrom parsedMsgs
            list  :: (Show t) => [t] -> String
            list = unlines . (map show)
            limit = if n > 0 then take n else id
        putStrLn $ list $ limit updateMsgs
        -- summary rib >>= putStrLn

getUpdatesFrom msgs = foldr keepOnlyUpdates [] msgs where
                          keepOnlyUpdates a@(BGPUpdate{}) ax = a:ax
                          keepOnlyUpdates _ ax = ax

dropUpdatesFrom msgs = foldr keepOnlyUpdates [] msgs where
                          keepOnlyUpdates a@(BGPUpdate{}) ax = ax
                          keepOnlyUpdates a ax = a:ax

processMsg rib msg = do
    case msg of 
        BGPTimeout -> do
            putStrLn "BGPTimeout"
        open@BGPOpen{} -> do
            putStrLn "BGPOpen"
            print open
        notify@BGPNotify{} -> do
           putStrLn "BGPNotify"
           print notify

        BGPKeepalive -> do
            putStrLn "BGPKeepalive"

        update@BGPUpdate{..} -> do
            parsedUpdate@(Just(parsedAttributes,parsedNlri,parsedWithdrawn)) <- processUpdate attributes nlri withdrawn verbose
            if isJust parsedUpdate then do
                ribUpdateMany rib (parsedAttributes,attributes) parsedNlri
                ribWithdrawMany rib parsedWithdrawn
                unless (L.null withdrawn)
                    ( summary rib >>= putStrLn )
                return ()
            else do
                putStrLn "Update parse error"
                print update
