{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Binary
import PathAttributes
import Control.Exception(assert)
import qualified Data.ByteString.Lazy as L
import Hexdump
simpleHex' = simpleHex . L.toStrict

main = do
    mapM_ testCode allPathAttributeTypeCodes
    test []
    test [PathAttributeOrigin 2]
    test [PathAttributeASPath (ASPath []) ]
    test [PathAttributeASPath (ASPath [ASSequence [1]]) ]
    test attrs1
    where
        testCode c = assert ( c == (toEnum . fromEnum) c ) (putStrLn $ show c ++ " - OK" )
        path0 = ASPath []
        path1 = ASPath [seg1]
        path2 = ASPath [seg1,seg2]
        seg1  = ASSet [1,2,3]
        seg2  = ASSequence [6,5,4]
        attrs1 = [PathAttributeOrigin 2, PathAttributeASPath path2, PathAttributeNextHop "192.168.0.1"]

test :: [PathAttribute] -> IO()
test pas = do putStrLn ""
              let enc = encode pas
                  dec = decode enc :: [PathAttribute]
              -- putStrLn $ "original: " ++ show pas
              -- putStrLn $ "encoded:  " ++ simpleHex' enc
              if dec == pas
              then putStrLn $ show pas ++ " OK"
              else do putStrLn "*** FAIL ***"
                      putStrLn $ "original: " ++ show pas
                      putStrLn $ "decoded:  " ++ show dec
                      putStrLn $ "encoded:  " ++ simpleHex' enc
              putStrLn " ------------------"


