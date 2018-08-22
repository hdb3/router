{-#LANGUAGE OverloadedStrings #-}
module Overlap where

import Prefixes

data Tree a = Tree a [Tree a]

type PrefixTree = Tree IPrefix

defaultRoute :: IPrefix
defaultRoute = "0.0.0.0/0"

root = Tree defaultRoute []

data Overlaps = Includes | IncludedBy | NoOverlap

comp :: IPrefix -> IPrefix -> Overlaps
comp a b = NoOverlap
