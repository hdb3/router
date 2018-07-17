-- # LANGUAGE MultiWayIf #-}
-- # LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import ASPath
-- import RFC4271
-- import Codes
-- import Common
import Data.Binary
-- import Data.Binary.Get
-- import Data.Binary.Put
-- import Data.Word
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
-- import Control.Monad
import Hexdump
simpleHex' = simpleHex . L.toStrict

main = do
    let path0 = ASPath []
        path1 = ASPath [seg1]
        path2 = ASPath [seg1,seg2]
        seg1  = ASSet [1,2,3]
        seg2  = ASSequence [6,5,4]
    mapM_ test [path0,path1,path2]

test :: ASPath -> IO()
test path = do putStrLn ""
               let enc = encode path
                   dec = decode enc :: ASPath
               print path
               putStrLn ""
               putStrLn $ "encoded: " ++ simpleHex' enc
               putStrLn $ "decoded: " ++ show dec
               putStrLn " ------------------"
