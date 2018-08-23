{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Overlap where
import Data.List(foldl')
import Data.Bits
import Prefixes

type PrefixTree = Node Prefix

data Overlaps = Includes | IncludedBy | NoOverlap | Equal deriving Eq
class Overlap a where
    comp :: a -> a -> Overlaps

instance Overlap Prefix where
    comp (Prefix (!l1,!v1)) (Prefix (!l2,!v2)) 
                                               | l1 == l2 && v1 == v2 = Equal
                                               | l1 == l2 = NoOverlap
                                               | l1 < l2 && mask l1 v1 == mask l1 v2 = Includes
                                               | l1 > l2 && mask l2 v1 == mask l2 v2 = IncludedBy 
                                               | otherwise = NoOverlap
                                               where mask l v = v .&. shiftR (fromIntegral l) 0xffffffff

data Node a = Empty | Item a (Node a) (Node a) deriving Show

singleton x = Item x Empty Empty
insert Empty x = singleton x
insert (Item a b c) x
                      | overlap == Equal = Item a b c
                      | overlap == NoOverlap  = Item a (insert b x) c
                      | overlap == Includes   = Item a b (insert c x)
                      | overlap == IncludedBy = Item x Empty (Item a b c )
                        where overlap = comp a x

fromList :: Overlap a => [a] -> Node a
fromList = foldl' insert Empty
