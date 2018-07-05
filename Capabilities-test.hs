{-# LANGUAGE MultiWayIf #-}
{-- LANGUAGE DataKinds #-}
module Main where
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as B
-- import Data.ByteString(ByteString)
-- import Data.Binary
-- import Data.Binary.Get
-- import Data.Binary.Put
-- import Data.ByteString.Builder
-- import Data.Monoid((<>))
import Capabilities
import Hexdump

main = do
    putStrLn "capability test"
    let capmp = CapMultiprotocol 0 0
        capAS4 = CapAS4 0x00010f0f
        capGrR = CapGracefulRestart False 0
        params = buildOptionalParameters [capmp, capAS4, capGrR]
    putStrLn $ simpleHex params
    let decoded = parseOptionalParameters params
    print decoded
{-
-}
    putStrLn "done"
