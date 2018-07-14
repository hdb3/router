{-# LANGUAGE MultiWayIf,FlexibleInstances,OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.Word
import Data.IP
import Hexdump
import Prefixes

main = do
    test "0.0.0.0/0"
    test "192.168.1.99/24"
    test "172.16.0.77/12"
    test "169.254.108.17/32"
    test "10.1.2.3/8"

test :: AddrRange IPv4 -> IO ()
test ar = do
    putStrLn $ "AddrRange: " ++ show ar
    let pfx = fromAddrRange ar
    putStrLn $ "prefix: " ++ show pfx

    let arMasked' = makeAddrRange (masked ip' (intToMask subnet')) subnet' where
        (ip',subnet') = addrRangePair ar
    putStrLn $ "AddrRange (masked): " ++ show ar

    let pfxMasked = canonicalPrefix pfx
    putStrLn $ "prefix (masked): " ++ show pfxMasked
    putStrLn "------------------"
    putStrLn ""

