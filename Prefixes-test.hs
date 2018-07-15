{-# LANGUAGE MultiWayIf,FlexibleInstances,OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.Word
import Data.IP
import Data.Binary
import Hexdump
import Prefixes
import Data.ByteString.Lazy(toStrict)
-- import Data.ByteString(ByteString,unpack)
import Data.ByteString.Char8(ByteString,unpack)
import qualified Data.ByteString.Base16

simpleHex' = simpleHex.toStrict
readPfx :: String -> Prefix
readPfx = fromAddrRange.read
toHex = (unpack).Data.ByteString.Base16.encode
fromHex = (fst).Data.ByteString.Base16.decode

main = do
    -- testDecode
    testEncode

testDecode = do
    mapM_ test1 $ map fromHex
        ["00"
        --,"0801"
        ]
    where
    test1 :: ByteString -> IO ()
    test1 bs = do
        putStrLn $ toHex bs
        --putStrLn $ show (encode bs)
        putStrLn "------------------"
        putStrLn ""


testEncode = do
    mapM_ test1
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
    test0 :: AddrRange IPv4 -> IO ()
    test0 ar = do
        let pfx = fromAddrRange ar
        putStrLn $ "prefix: " ++ show pfx
        let enc = encode pfx
        putStrLn $ "encoded: " ++ (simpleHex' enc)
        putStrLn "------------------"
        putStrLn ""
    
    test1 :: AddrRange IPv4 -> IO ()
    test1 ar = do
        let pfx = fromAddrRange ar
        putStrLn $ "prefix: " ++ show pfx
        let enc = encode pfx
        putStrLn $ "encoded: " ++ (simpleHex' enc)
{-
        let dec = decode enc
        if dec == enc then putStrLn "OK"
                      else do putStrLn "*** FAIL ***"
                              putStrLn $ "encoded: " ++ (simpleHex' enc)
                              putStrLn $ "decoded: " ++ show dec
-}    
        putStrLn "------------------"
        putStrLn ""
    
    test2 ar = do
        putStrLn $ "AddrRange: " ++ show ar
        let pfx = fromAddrRange ar
        putStrLn $ "prefix: " ++ show pfx
        let ar' = toAddrRange pfx
        putStrLn $ "AddrRange': " ++ show ar
    
        let arMasked' = makeAddrRange (masked ip' (intToMask subnet')) subnet' where
            (ip',subnet') = addrRangePair ar
        -- putStrLn $ "AddrRange (masked): " ++ show ar
    
        let pfxMasked = canonicalPrefix pfx
        -- putStrLn $ "prefix (masked): " ++ show pfxMasked
        putStrLn "------------------"
        putStrLn ""
