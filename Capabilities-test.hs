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

capmp = CapMultiprotocol 0 0
capAS4 = CapAS4 0x00010f0f
capGrR = CapGracefulRestart False 0
testList = [
             ("empty list",[]),
             ("singleton list",[capGrR]),
             ("long list",[capmp, capAS4, capGrR])
           ] :: [(String,[Capability])]

main = do
    putStrLn "capability test"
    testEncodings
    putStrLn ""
    runTests "testOptionalParameters" testOptionalParameters testList

testEncodings (d,ps) = do
    putStrLn "testEncodings"
        putStrLn $ d ++ ":" ++ show ps
        mapM t ps where
        t p = do
            let encodedCapability = encode p
                decodedCapability = decode encodedCapability
            putStrLn $ "encoded: " ++ simpleHex codedCapability
            putStrLn $ "decoded: " ++ show decodedCapability
            if (decodedCapability == p) then
                putStrLn "conversion OK" else
                putStrLn "*** conversion FAIL!!!"
            putStrLn ""
    putStrLn "done"

runTests desc f tests = do
    putStrLn desc
    mapM  f tests
    putStrLn "done"

testOptionalParameters (d,ps) = do
        let params = buildOptionalParameters ps
            recodedParams = parseOptionalParameters params
        putStrLn $ d ++ ":" ++ show ps
        putStrLn $ "encoded: " ++ ( simpleHex params)
        putStrLn $ "decoded: " ++ show recodedParams
        if (recodedParams == ps) then
            putStrLn "conversion OK" else
            putStrLn "*** conversion FAIL!!!"
        putStrLn ""
