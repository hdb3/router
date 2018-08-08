module PathTable where

{- The path table holds everything about a route we could care about
 -
 - the contents are the actual route itself, (parsed form and the bytestring hash actually required),
 - a hash, for uniquness checking (Assume that hashing the whole bytestring is valid....)
 - a refernce count,
 - and an opaque record for other data, e.g calculated cost
-}

import Data.IntMap.Strict
import PathAttributes
import FarmHash(hash64)

import BGPData

data PathTableEntry = PathTableEntry { ptePath :: [PathAttribute], pteData :: RouteData, refCount :: Int }
newtype PathTable = PathTable IntMap PathTableEntry

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
