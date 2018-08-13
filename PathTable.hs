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

import qualified Data.ByteString as B
import Data.IntMap.Strict
import PathAttributes
import FarmHash(hash64) -- from package farmhash

import BGPData

data PathTableEntry = PathTableEntry { ptePath :: [PathAttribute], pteData :: RouteData, refCount :: Int }

newtype PathTable = PathTable (IntMap PathTableEntry)
newtype RouteId = RouteId Int

pathTableGet :: PathTable -> RouteId -> Maybe PathTableEntry
pathTableGet (PathTable pt) (RouteId routeId) = Data.IntMap.Strict.lookup routeId pt 

pathTableGet_ :: PathTable -> RouteId -> PathTableEntry
pathTableGet_ (PathTable pt) (RouteId routeId) = pt ! routeId

pathTableDelete :: PathTable -> RouteId -> Int -> PathTable
pathTableDelete (PathTable pt)(RouteId routeId) count = PathTable $ update f routeId pt where -- update cannot insert....
    f oldPt | refCount oldPt == count = Nothing
            | otherwise = Just $ oldPt { refCount = (refCount oldPt) - count }
 
pathTableInsert :: PathTable -> ([PathAttribute],B.ByteString) -> Int -> RouteData -> (RouteId,PathTable)

pathTableInsert (PathTable pt) (route,bytes) pfxCount routeData = (RouteId hash,pt') where
    hash = fromIntegral $ hash64 bytes
    pte = PathTableEntry route routeData pfxCount
    pt' = PathTable (alter f hash pt) where -- alter  CAN do an insert - (tho it connot report whether it did or not...)
       f = maybe (Just ( PathTableEntry route routeData pfxCount))
                 (\oldPte -> Just ( oldPte { refCount = (refCount oldPte) + pfxCount }))
