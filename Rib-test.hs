{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Binary
import Data.ByteString.Lazy(toStrict)

import Prefixes
import PathAttributes
import Rib


prefix1 = fromAddrRange "172.16.0.77/12"
prefix2 = fromAddrRange "192.168.122.1/16"
prefix3 = fromAddrRange "10.0.0.0/8"
prefix4 = fromAddrRange "10.0.4.0/16"
prefix5 = fromAddrRange "10.0.5.0/16"
prefix6 = fromAddrRange "10.0.6.0/16"
prefix7 = fromAddrRange "10.0.7.0/16"
prefix8 = fromAddrRange "10.0.8.0/16"
prefix9 = fromAddrRange "10.0.9.0/16"
prefix0 = fromAddrRange "10.0.11.0/16"
path1 = ASPath [ ASSequence [6,5,4] ]
path2 = ASPath [ ASSequence [6,5,4,3,2,1] ]
attrs1 = [PathAttributeOrigin 2, PathAttributeASPath path1, PathAttributeNextHop "192.168.0.1"]
attrs2 = [PathAttributeOrigin 1, PathAttributeASPath path2, PathAttributeNextHop "192.168.0.2"]
pathA = (attrs1, toStrict $ encode attrs1)
pathB = (attrs2, toStrict $ encode attrs2)

pr rib = do s <- display rib
            putStrLn s

diff rib1 rib2 = do
    r1 <- display rib1
    r2 <- display rib2
    return $ r1 == r2

main' = do
    rib <- newRib
    pr rib
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix1
    pr rib
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix2
    pr rib
    ribWithdraw rib prefix1
    pr rib
    ribWithdraw rib prefix2
    pr rib
    ribWithdraw rib prefix2
    pr rib

main'' = do
    rib <- newRib
    rib' <- newRib
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix2
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix1
    ribUpdateMany rib' (attrs1, toStrict $ encode path1) [prefix1,prefix2]
    pr rib
    pr rib'
    p <- diff rib rib'
    print p
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix3
    p <- diff rib rib'
    print p

main = do
    rib <- newRib
    ribUpdateMany rib pathA [prefix1,prefix2]
    ribUpdateMany rib pathB [prefix3,prefix4]
    pr rib
    ribUpdateMany rib pathB [prefix1,prefix2]
    pr rib
    ribWithdrawMany rib [prefix4,prefix3]
    pr rib
    ribWithdrawMany rib [prefix2,prefix1]
    pr rib
