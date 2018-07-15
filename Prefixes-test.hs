{-# LANGUAGE MultiWayIf,FlexibleInstances,OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.Word
import Data.IP
import Data.Binary
import Hexdump
import Prefixes
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Base16.Lazy as Base16

simpleHex' = simpleHex.(L.toStrict)
readPfx :: String -> Prefix
readPfx = fromAddrRange.read
toHex = (LC.unpack).Base16.encode
fromHex = (fst).Base16.decode
decodePrefix :: L.ByteString -> Prefix
decodePrefix = decode
encodePrefix :: Prefix -> L.ByteString
encodePrefix = encode
-- pfx = decode bs :: Prefix
p = do putStrLn "------------------" ; putStrLn ""

main = do
    testDecode
    testEncode

testDecode = do
    mapM_ test1 $ map fromHex
        ["00"
        ,"0801"
        ,"100102"
        ,"18010203"
        ,"1c01020340"
        ,"2001020304"
        ]
    where
    test1 :: L.ByteString -> IO ()
    test1 bs = do
        putStrLn $ toHex bs
        let pfx = decode bs :: Prefix
        putStrLn $ show pfx
        putStrLn "------------------"
        putStrLn ""


testEncode = do
    let test :: Prefix -> IO ()
        test = test3
    mapM_ test $ map fromAddrRange
        ["1.2.3.4/32"
        ,"1.2.3.4/24"
        ,"1.2.3.4/16"
        ,"1.2.3.4/8"
        , "0.0.0.0/0"
        , "192.168.1.99/24"
        , "129.129.0.0/16"
        , "172.16.0.77/12"
        , "169.254.108.17/32"
        , "10.1.2.3/8"
        ]
    where
    test0 pfx = do
        putStrLn $ "prefix: " ++ show pfx
        let enc = encodePrefix pfx
        putStrLn $ "encoded: " ++ (simpleHex' enc)
    
    test1 pfx = do
        putStrLn $ "prefix: " ++ show pfx
        let enc = encodePrefix pfx
        putStrLn $ "encoded: " ++ (simpleHex' enc)
        p
    
    test2 pfx = do
        -- putStrLn $ "AddrRange: " ++ show ar
        -- let pfx = fromAddrRange ar
        putStrLn $ "prefix: " ++ show pfx
        let ar = toAddrRange pfx
        putStrLn $ "AddrRange': " ++ show ar
    
        let arMasked' = makeAddrRange (masked ip' (intToMask subnet')) subnet' where
            (ip',subnet') = addrRangePair ar
        -- putStrLn $ "AddrRange (masked): " ++ show ar
    
        let pfxMasked = canonicalPrefix pfx
        -- putStrLn $ "prefix (masked): " ++ show pfxMasked
        p

    test3 pfx = do
        putStrLn $ "prefix: " ++ show pfx
        let enc = encodePrefix pfx
        let dec = decodePrefix enc
        putStrLn $ "encoded: " ++ (simpleHex' enc)
        putStrLn $ "decoded: " ++ show dec
        if dec == pfx then putStrLn "OK"
                      else do putStrLn "*** FAIL ***"
                              putStrLn $ "encoded: " ++ (simpleHex' enc)
                              putStrLn $ "decoded: " ++ show dec

        p
