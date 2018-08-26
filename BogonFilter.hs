{-# LANGUAGE OverloadedStrings #-}
module BogonFilter where
-- import System.IO
-- import qualified Data.List
import Data.IP

-- import Common
import Prefixes
-- import BGPReader(readRib,Rib)
-- import qualified Overlap
-- import qualified PathAttributes

applyBogonFilter :: [(a, [Prefix])] -> [(a, [Prefix])]
applyBogonFilter = filter p . map f where
    f (a,pfxs) = (a, filter bogonFilter pfxs)
    -- f (a,pfxs) = (a, filter (not . bogonFilter) pfxs)
    p (a,[])   = False
    p _          = True
    p' = not . p

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
