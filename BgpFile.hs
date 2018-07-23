{-# LANGUAGE RecordWildCards #-}
module Main where
import System.Environment
-- import Network.Socket
-- import System.IO.Error(catchIOError)
import System.IO(Handle,openBinaryFile,IOMode(..))
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
-- import Data.Binary.Strict
-- import Data.Binary(encode,decode,decodeOrFail)
import Data.Binary(Binary(..))
import Data.Binary.Get(runGet)
-- import Control.Concurrent
-- import Control.Exception
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust)
-- import Data.Either(either)
-- import Data.Int(Int64)

import Common
import BGPparse
import GetBGPMsg
-- import RFC4271
-- import Open
-- import Capabilities
-- import Collision
import Update
import PathAttributes
import Prefixes
import Rib

verbose = True 

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
        let msgs' = if n > 0 then take n msgs else msgs
        mapM_ (processMsg rib) msgs'

processMsg rib msg = do
    let msg' = decodeBGPByteString msg
    case msg' of 
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
            else do
                putStrLn "Update parse error"
                print update
