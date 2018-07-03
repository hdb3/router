{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Word
import Data.ByteString.Lazy(fromStrict,toStrict)
import qualified Data.ByteString as B
import TLV
import Hexdump

testData = [(1,"garbage"),
            (1,"in"),
            (5,"GARBAGE"),
            (255,"OUT")]

main = do 
    print testData
    --let testDataS' = toStrict $ innerDeparser testData
    --putStrLn $ simpleHex testDataS'
    --print $ innerParser testDataS'
    --putStrLn "---------------------------"
    let testDataS = outerDeparser testData
    putStrLn $ simpleHex testDataS
    putStrLn $ prettyHex testDataS
    check testData testDataS
    check testData (B.init testDataS)
    check testData (B.tail testDataS)
{-
    either
        (\s -> putStrLn $ "parse failed" ++ s)
        (\respData -> do putStrLn "parse success"
                         putStrLn $ "identity result: " ++ show (respData == testData)
                         print respData)
        (openParser testDataS)
-}
check call resp = 
    either
        (\s -> putStrLn $ "parse failed" ++ s)
        (\respData -> do putStrLn "parse success"
                         putStrLn $ "identity result: " ++ show (respData == call)
                         print respData)
        (openParser resp)
