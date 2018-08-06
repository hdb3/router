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

data Cost = Cost { preference :: Word32, pathLength :: Word8, med :: Word32, fromEBGP :: Bool, peerBGPID :: Word32 }
instance Ord Cost where
  compare (Cost pr1 pl1 med1 fEBGP1 peer1)(Cost pr2 pl2 med2 fEBGP2 peer2)
      | pr1 /= pr2 = compare pr1 pr2
      | pl1 /= pl2 = compare pl1 pl2
      | med1 /= med2 = compare med1 med2
      | fEBGP1 /= fEBGP2 = compare fEBGP1 fEBGP2
      | otherwise = compare peer1 peer2
 
data PathTableEntry = PathTableEntry { ptePath :: [PathAttribute], pteCost :: Cost, pteNewPath [PathAttribute] }
newtype PathTable = PathTable { map :: IntMap PathTableEntry } -- ignoring cost function
type UpdatePathFunction = ([PathAttribute] -> [PathAttribute])
makeSimpleUpdatePathFunction :: Word32 -> ASPath2  -> UpdatePathFunction -- TODO - support AS2 and AS4.....
makeSimpleUpdatePathFunction nextHop addAS = (updateNextHop nextHop) (addAS newAS) 
-- newtype PathTable = PathTable { map :: IntMap PathTableEntry, costFunction :: CostFunction
-- type CostFunction = [PathAttribute] -> Cost
-- makeCostFunction :: ([PathAttribute] -> Word32) -> Word32 -> CostFunction
-- makeCostFunction prefFunction peerBGPID = f where
--     f path = 

{- PathTable insert: provide the prefix count so ref count can be maintained.
 -  This also updates the ref count for the dereferenced paths.
 -  Unreferenced paths are removed.  A path table reference is returned.
-}

pathTableInsert :: PathTable -> ([PathAttribute],B.ByteString) -> Int -> (Int,PathTable)
-- in this simple version we are not going to bother about removing old path entries and managing reference counts....
--
pathTableInsert pt (route,bytes) pfxCount = (hash,pt') where
    hash = hash64 bytes
    pt' = 
