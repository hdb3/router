
module PathData where

{- PathData holds information needed for route selection, filtering, etc that is specific to a route
data Cost = Cost { preference :: Word32, pathLength :: Word8, med :: Word32, fromEBGP :: Bool, peerBGPID :: Word32 }
instance Ord Cost where
  compare (Cost pr1 pl1 med1 fEBGP1 peer1)(Cost pr2 pl2 med2 fEBGP2 peer2)
      | pr1 /= pr2 = compare pr1 pr2
      | pl1 /= pl2 = compare pl1 pl2
      | med1 /= med2 = compare med1 med2
      | fEBGP1 /= fEBGP2 = compare fEBGP1 fEBGP2
      | otherwise = compare peer1 peer2
 
data PathTableEntry = PathTableEntry { ptePath :: [PathAttribute], pteCost :: Cost, pteNewPath [PathAttribute] }
newtype PathTable = PathTable { map :: IntMap PathTableEntry, updateFunction :: UpdatePathFunction } -- ignoring cost function
type UpdatePathFunction = ([PathAttribute] -> [PathAttribute])
makeSimpleUpdatePathFunction :: Word32 -> ASNumber -> UpdatePathFunction -- TODO - support AS2 and AS4.....
makeSimpleUpdatePathFunction nextHop newAS = (updateNextHop nextHop) (prePendAS newAS) 

data PeerConfig = PeerConfig {preference :: Word32,peerBGPID :: Word32, myAS :: ASNumber}

pathTableInsert :: PathTable -> ([PathAttribute],B.ByteString) -> Int -> (Int,PathTable)
-- in this simple version we are not going to bother about removing old path entries and managing reference counts....
