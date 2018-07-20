{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Binary
import Data.ByteString.Lazy(toStrict)

import Prefixes
import PathAttributes
import Rib


prefix1 = fromAddrRange "172.16.0.77/12"
prefix2 = fromAddrRange "192.168.122.1/16"
path1 = ASPath [ ASSequence [6,5,4] ]
attrs1 = [PathAttributeOrigin 2, PathAttributeASPath path1, PathAttributeNextHop "192.168.0.1"]
pr rib = do s <- display rib
            putStrLn s
main = do
    rib <- newRib
    pr rib
    ribUpdate rib prefix1 (attrs1, toStrict $ encode path1)
    pr rib
    ribUpdate rib prefix2 (attrs1, toStrict $ encode path1)
    pr rib
    ribWithdraw rib prefix1
    pr rib
    ribWithdraw rib prefix2
    pr rib
    ribWithdraw rib prefix2
    pr rib


-- ribUpdate :: Rib -> Prefix -> ([PathAttribute],ByteString) -> IO()
-- ribWithdraw :: Rib -> Prefix -> IO()
