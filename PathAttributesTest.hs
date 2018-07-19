{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Binary
import Control.Exception(assert)
import qualified Data.ByteString.Lazy as L
import Data.Int(Int64)

import Common
import PathAttributes

main = do
    main'
    test [PathAttributeLargeCommunity[(1::Word32,2::Word32,3::Word32)]]
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
test pas = do
              putStrLn ""
              let enc = encode pas
                  dec = decode enc :: [PathAttribute]
              putStrLn $ "original: " ++ show pas
              putStrLn $ "encoded:  " ++ simpleHex' enc
              if dec == pas
              then putStrLn $ show pas ++ " OK"
              else do putStrLn "*** FAIL ***"
                      putStrLn $ "original: " ++ show pas
                      putStrLn $ "decoded:  " ++ show dec
                      putStrLn $ "encoded:  " ++ simpleHex' enc
              putStrLn " ------------------"

main' = decodeAttributes $ fromHex' "40010100500200100207fbf563740cb900ae5ba051cc51cc400304c0a87af0c008140cb91f630cb975ca0cb9c3510cb9d4800cb9d481d011001e02070000fbf50000637400000cb9000000ae000320b3000051cc000051cce02030000320b30000000100000001000320b30000000200000001000320b30000000300000004000320b300000004000051cc"

decodeAttributes :: L.ByteString -> IO()
decodeAttributes ps = 
    either
        (\(_,offset,msg) -> do
            putStrLn $ "failed at offset " ++ show offset
            putStrLn $ prettyHex' ps
        )
        (\(_,_,attributes) -> do
            putStrLn "success"
            putStrLn "attributes"
            print attributes
        )
        (decodeOrFail ps :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [PathAttribute]))
