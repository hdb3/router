{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prefixes
import Overlap2

fl :: [Prefix] -> PrefixTree
fl = fromList
-- fl = fromList . ("0.0.0.0/0" :)

ins = flip insertPrefix
main = do
    print $ fromList [ "128.0.0.0/1" , "0.0.0.0/0"]
    print $ map fromList l
    print $ map fromList m

main' = do
    -- t id
    -- t width
    -- t size
    -- t (\x -> (id x,size x))
    let f x = (id x,size x,width x, height x)
        trees = map ( f . fl) m
    mapM print trees

l = [
       ["0.0.0.0/0"]
     , ["192.168.0.0/24"]
     , ["192.168.0.0/24","192.168.0.0/24"]
     , ["192.168.1.0/24","192.168.0.0/24"]
     , ["192.168.1.0/24","192.168.1.128/25"]
    ]

m = [ ["192.168.0.0/24"]
    , ["192.168.0.0/24","192.168.1.0/24","192.168.2.0/24"]
    , ["192.168.1.0/24","192.168.1.0/25","192.168.1.0/26"]
    , ["192.168.0.0/24","192.168.1.0/24","192.168.2.0/24","192.168.2.0/25","192.168.2.0/26"]
    ]
