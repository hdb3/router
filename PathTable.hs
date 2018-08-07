module PathTable where

{- The path table holds everything about a route we could care about
 - this includes reference counts, though arguably this could be expensive and better to ignore until ready to optimise
 - refernce counts are only needed to ensure routes don't accumulate.
 - it would be possible to do this with a background task, locking the table occasionally
 -
 - the main contents are the actual route itself, (parsed form and the bytestring hash actually required),
 - the calculated prefernce and other selection values,
 - a hash, for uniquness checking (Assume that hashing the whole bytestring is valid....)
 - and finally the updated route for dissemination (maybe two versions for internal and external peers...)
-}

import Data.IntMap.Strict
import PathAttributes
import FarmHash(hash64)

import PathData

data PathTableEntry = PathTableEntry { ptePath :: [PathAttribute], pteData :: PathData, refCount :: Int }
newtype PathTable = PathTable IntMap PathTableEntry

pathTableDelete :: PathTable -> Int -> Int -> PathTable
pathTableDelete pt hash count = update f pt hash where
    f oldPt | refCount oldPt == count = Nothing
            | otherwise = oldPt { refCount = oldCount - count }
 
pathTableInsert :: PathTable -> ([PathAttribute],B.ByteString) -> Int -> PathData -> (Int,PathTable)
-- in this simple version we are not going to bother about removing old path entries and managing reference counts....
--
pathTableInsert (PathTable pt) (route,bytes) pfxCount pathData = (hash,pt') where
    hash = hash64 bytes
    pte = PathTableEntry route pathData pfxCount
    pt' = PathTable (alter f hash pt) where
       f = maybe (Just ( PathTableEntry route pathData pfxCount))
                 (\oldPt -> Just ( oldPt { refCount = oldCount + count }))
