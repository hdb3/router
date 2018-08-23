{-#LANGUAGE OverloadedStrings #-}
module Main where
import Prefixes
import Overlap

fl :: [Prefix] -> PrefixTree
fl = fromList . ("0.0.0.0/0" :)

main = do
    print $ fl []
    print $ singleton "192.168.0.0/24"
    print $ fl ["192.168.0.0/24"]
    print $ fl ["192.168.0.0/24","192.168.0.0/24"]
    print $ fl ["192.168.1.0/24","192.168.0.0/24"]
    print $ fl ["192.168.1.0/24","192.168.1.128/25"]
