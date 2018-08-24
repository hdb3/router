{-#LANGUAGE OverloadedStrings #-}
module Overlap2(Tree(Empty),PrefixTree,height,size,fromList,insertPrefix) where
import Data.List(foldl')
import Data.Bits(testBit)
import Data.Word
import Prefixes

type PrefixTree = Tree Prefix

data Tree a = Empty | Item (Maybe a) (Tree a) (Tree a)
instance Show a => Show (Tree a) where
    show = showN where

        showRL Empty = ""
        showRL (Item (Just a) b c) = "[" ++ show a ++ "]" ++ show b ++ show c
        showRL (Item Nothing Empty c) = "R" ++ show c
        showRL (Item Nothing b Empty) = "L" ++ show b
        showRL (Item Nothing b c) = "RL" ++ show b ++ show c

        showN (Item (Just a) b c) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"
        -- show (Item Nothing Empty c) = show c
        -- show (Item Nothing b Empty) = show b
        showN (Item Nothing b c) = "(*," ++ show b ++ "," ++ show c ++ ")"
        showN Empty = "-"

isSet :: Word8 -> Word32 -> Bool
isSet level bits = testBit bits (31 - fromIntegral level)

ins (a,bits,target,level) Empty        | level == target  = Item (Just a) Empty Empty
                                       | isSet level bits = Item Nothing (ins (a,bits,target,level+1) Empty) Empty
                                       | otherwise        = Item Nothing Empty (ins (a,bits,target,level+1) Empty) 
ins (a,bits,target,level) (Item x y z) | level == target  = Item (Just a) y z
                                       | isSet level bits = Item x (ins (a,bits,target,level+1) y) z
                                       | otherwise        = Item x y (ins (a,bits,target,level+1) z) 

insert a bits target = ins (a,bits,target,0)

insertPrefix :: Prefix -> PrefixTree -> PrefixTree
insertPrefix pfx@(Prefix (l,v)) t = ins (pfx,v,l,0) t

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Item Nothing b c) = Item Nothing (fmap f b) (fmap f c)
    fmap f (Item (Just a) b c) = Item (Just (f a)) (fmap f b) (fmap f c)

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Item Nothing b c ) = foldr f (foldr f z b) c
    foldr f z (Item (Just a) b c ) = foldr f (foldr f (f a z) b) c

singleton x = Item (Just x) Empty Empty

fromList :: [Prefix] -> PrefixTree
fromList = foldl' (flip insertPrefix) Empty

size :: Tree a -> Int
size = foldr (\_ b -> b+1) 0

height :: Tree a -> Int
height Empty = 0
height (Item Nothing b c) = max (height b) (height c)
height (Item _ b c) = 1 + max (height b) (height c)
