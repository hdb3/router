module Prefix where

class Prefix a where
  toInt :: a -> Int
  fromInt :: Int -> a
