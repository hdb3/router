{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Binary
import qualified Data.ByteString.Lazy as L

import Common
import ASPath

asSequence2 = ASSequence . as2list
asSet2 = ASSet . as2list
asSequence4 = ASSequence . as4list
asSet4 = ASSet . as4list
main = do
    let path0 = ASPath [] :: ASPath2
        path1 = ASPath [seg0] :: ASPath2
        path2 = ASPath [seg1] :: ASPath2
        path3 = ASPath [seg1,seg2] :: ASPath2
        seg0  = asSequence2 []
        seg1  = asSet2 [1,2,3]
        seg2  = asSequence2 [6,5,4]
    -- encDecSeq seg0
    -- encDecSeq seg1
    -- test path0
    -- test path1
    -- test ( ASPath [seg0] :: ASPath2)
    -- test ( ASPath [ seg0, seg0] :: ASPath2)
    mapM_ test2 [path0,path1,path2,path3]
    let path40 = ASPath [] :: ASPath4
        path41 = ASPath [seg4a] :: ASPath4
        path42 = ASPath [seg4b] :: ASPath4
        path43 = ASPath [seg4c] :: ASPath4
        path44 = ASPath [seg4b,seg4c] :: ASPath4
        seg4a  = asSequence4 []
        seg4b  = asSet4 [1,2,3]
        seg4c  = asSequence4 [6,5,4]
    mapM_ test4 [path40,path41,path42,path43,path44]
    let path42' = asPrePend 42 path42
        path2'  = asPrePend 42 path2
        path43' = asPrePend 42 path43
        path3'  = asPrePend 42 path3
    print path42'
    print path2'
    print path43'
    print path3'

-- encDecSeq :: ASNumber asn => ASSegment asn -> IO ()
encDecSeq seq = do
    let enc = encode seq
        dec = decode enc
        ok = seq == dec
    putStrLn ""
    print ok
    putStrLn $ "original: " ++ show seq
    putStrLn $ "encoded:  " ++ simpleHex' enc
    putStrLn $ "decoded:  " ++ show dec
    putStrLn " ------------------"

test2 :: ASPath2 -> IO()
test2 path = do
               putStrLn ""
               let enc = encode path
                   dec = decode enc :: ASPath2
               print path
               putStrLn ""
               putStrLn $ "encoded: " ++ simpleHex' enc
               putStrLn $ "decoded: " ++ show dec
               putStrLn " ------------------"

test4 :: ASPath4 -> IO()
test4 path = do
               putStrLn ""
               let enc = encode path
                   dec = decode enc :: ASPath4
               print path
               putStrLn ""
               putStrLn $ "encoded: " ++ simpleHex' enc
               putStrLn $ "decoded: " ++ show dec
               putStrLn " ------------------"
