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
path1 = ASPath [ ASSequence [6,5,4] ]
attrs1 = [PathAttributeOrigin 2, PathAttributeASPath path1, PathAttributeNextHop "192.168.0.1"]
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

main = do
    rib <- newRib
    rib' <- newRib
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix2
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix1
    ribUpdate rib (attrs1, toStrict $ encode path1) prefix3
    ribUpdateMany rib' (attrs1, toStrict $ encode path1) [prefix1,prefix2]
    pr rib
    pr rib'
    p <- diff rib rib'
    print p
