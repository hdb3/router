{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Overlap where
import Data.Bits
import Prefixes

data Tree a = Tree a [Tree a]

type PrefixTree = Tree Prefix

defaultRoute :: Prefix
defaultRoute = "0.0.0.0/0"

root = Tree defaultRoute []

data Overlaps = Includes | IncludedBy | NoOverlap

comp :: Prefix -> Prefix -> Overlaps
comp (Prefix (!l1,!v1)) (Prefix (!l2,!v2)) | l1 == l2 = NoOverlap
                                           |  l1 < l2 && (mask l1 v1) == ( mask l1 v2) = Includes
                                           |  l1 > l2 && (mask l2 v1) == ( mask l2 v2) = IncludedBy 
                                           | otherwise = NoOverlap
                                           where mask l v = v .&. (shiftR (fromIntegral l) 0xffffffff) 

insert x (Tree a 
