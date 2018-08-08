module PrefixTable where

{- A single prefix table holds everything about a prefix we could care about
 - but, this is merely the prefix itself, and the associated path
 -
 - for IPv4 the prefix including length fits in a 64 bit word, so can be the actual key
 - though it might be that a simple scarmable operation would make a better key for a tree...
 - Note also that the pathtable key is also a 64 bit word, so a map of Ints is all that is required....
 -
 - However, the LocRIB needs to access every prefix table when performing selection
-}

import Data.IntMap.Strict
import Data.SortedList

data PathTableEntry = PathTableEntry { ptePath :: [PathAttribute], pteData :: RouteData, refCount :: Int }
newtype PrefixTable = PrefixTable IntMap Int

pathTableDelete :: PathTable -> Int -> Int -> PathTable
pathTableDelete pt hash count = update f pt hash where
    f oldPt | refCount oldPt == count = Nothing
            | otherwise = oldPt { refCount = oldCount - count }
 
pathTableInsert :: PathTable -> ([PathAttribute],B.ByteString) -> Int -> RouteData -> (Int,PathTable)

pathTableInsert (PathTable pt) (route,bytes) pfxCount routeData = (hash,pt') where
    hash = hash64 bytes
    pte = PathTableEntry route pathData pfxCount
    pt' = PathTable (alter f hash pt) where
       f = maybe (Just ( PathTableEntry route routeData pfxCount))
                 (\oldPt -> Just ( oldPt { refCount = oldCount + count }))
