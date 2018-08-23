{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Overlap2(Tree(Empty),PrefixTree,height,width,size,fromList,insertPrefix) where
import Data.List(foldl')
import Data.Bits(testBit)
import Data.Word
import Prefixes

type PrefixTree = Tree Prefix

data Tree a = Empty | Item (Maybe a) (Tree a) (Tree a) deriving Show

isSet :: Word8 -> Word32 -> Bool
isSet level = testBit' level . fromIntegral where
    testBit' level = testBit (32 - (fromIntegral level :: Int))


ins (a,bits,target,level) Empty        | level == target  = Item (Just a) Empty Empty
                                       | isSet level bits = Item Nothing (ins (a,bits,target,level+1) Empty) Empty
                                       | otherwise        = Item Nothing Empty (ins (a,bits,target,level+1) Empty) 
ins (a,bits,target,level) (Item x y z) | level == target  = Item (Just a) y z
                                       | isSet level bits = Item x (ins (a,bits,target,level+1) y) z
                                       | otherwise        = Item x y (ins (a,bits,target,level+1) z) 

insert a bits target t = ins (a,bits,target,0) t 

insertPrefix :: PrefixTree -> Prefix -> PrefixTree
-- insertPrefix t pfx@(Prefix (l,v)) = Empty
insertPrefix t pfx@(Prefix (l,v)) = ins (pfx,v,l,0) t


instance Functor Tree where
    fmap _ Empty = Empty
    -- fmap f (Item a b c ) = Item (f a) (fmap f b) (fmap f c)  

instance Foldable Tree where
    foldr f z Empty = z
    -- foldr f z (Item a b c ) = foldr f (foldr f (f a z) b) c

singleton x = Item (Just x) Empty Empty

fromList :: [Prefix] -> PrefixTree
fromList = foldl' insertPrefix Empty

size :: Tree a -> Int
size _ = 0

width :: Tree a -> Int
width _ = 0

height :: Tree a -> Int
height _ = 0
