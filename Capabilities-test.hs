{-# LANGUAGE MultiWayIf #-}
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
    testEncodings
    putStrLn ""
    testOptionalParameters

testEncodings = do
    putStrLn "testEncodings"
    putStrLn "done"

testOptionalParameters = do
    putStrLn "testOptionalParameters"
    let capmp = CapMultiprotocol 0 0
        capAS4 = CapAS4 0x00010f0f
        capGrR = CapGracefulRestart False 0
        runtest d ps = do
            let params = buildOptionalParameters ps
            putStrLn $ d ++ ":" ++ show ps
            putStrLn $ "encoded: " ++ ( simpleHex params)
            putStrLn $ "decoded: " ++ ( show $ parseOptionalParameters params )
            putStrLn ""
    runtest "empty list" []
    runtest "singleton list" [capGrR]
    runtest "long list" [capmp, capAS4, capGrR]
    putStrLn "done"
