{-#LANGUAGE OverloadedStrings #-}
module Main where
import Prefixes
import Overlap

fl :: [Prefix] -> PrefixTree
fl = fromList . ("0.0.0.0/0" :)

main = do
    t id
    t size
    t (\x -> (id x,size x))

t f = do
    print $ f $ fl []
    print $ f $ singleton "192.168.0.0/24"
    print $ f $ fl ["192.168.0.0/24"]
    print $ f $ fl ["192.168.0.0/24","192.168.0.0/24"]
    print $ f $ fl ["192.168.1.0/24","192.168.0.0/24"]
    print $ f $ fl ["192.168.1.0/24","192.168.1.128/25"]
