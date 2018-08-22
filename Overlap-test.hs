{-#LANGUAGE OverloadedStrings #-}
module Main where
import Prefixes
import Overlap

main = do
    print $ singleton "192.168.0.0/24"
