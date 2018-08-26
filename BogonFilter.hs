{-# LANGUAGE OverloadedStrings #-}
module BogonFilter where
import Data.IP

import Prefixes

applyBogonFilter :: [(a, [Prefix])] -> [(a, [Prefix])]
applyBogonFilter = filter p . map f where
    f (a,pfxs) = (a, filter bogonFilter pfxs)
    p (a,[])   = False
    p _          = True
    p' = not . p

iPrefixBogonFilter :: IPrefix -> Bool
iPrefixBogonFilter = bogonFilter . toPrefix

bogonFilter :: Prefix -> Bool
bogonFilter pfx
                | "10.0.0.0/8" >:> ip = False
                | "172.16.0.0/12" >:> ip = False
                | "192.168.0.0/16" >:> ip = False
                | 25 < s = False
                | 0 == s = False
                | otherwise = True
    where ip = toAddrRange pfx
          s  = subnet pfx
