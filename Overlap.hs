{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Overlap where
import Data.Bits
import Prefixes

data Tree a = Tree a [Tree a]

type PrefixTree = Tree Prefix

defaultRoute :: Prefix
defaultRoute = "0.0.0.0/0"

-- root = Tree defaultRoute []

data Overlaps = Includes | IncludedBy | NoOverlap deriving Eq

comp :: Prefix -> Prefix -> Overlaps
comp (Prefix (!l1,!v1)) (Prefix (!l2,!v2)) | l1 == l2 = NoOverlap
                                           | l1 < l2 && mask l1 v1 == mask l1 v2 = Includes
                                           | l1 > l2 && mask l2 v1 == mask l2 v2 = IncludedBy 
                                           | otherwise = NoOverlap
                                           where mask l v = v .&. shiftR (fromIntegral l) 0xffffffff

-- insert logic

-- insert x (Tree a 

data Node a = Empty | Item a (Node a) (Node a) deriving Show
root = Item defaultRoute Empty Empty

singleton x = Item x Empty Empty
insert Empty x = singleton x
insert (Item a b c) x | overlap == NoOverlap  = Item a (insert b x) c
                      | overlap == Includes   = Item a b (insert c x)
                      | overlap == IncludedBy = Item x Empty (Item a b c )
                        where overlap = comp x a
