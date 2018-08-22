module AdjRIBOut where

{-
 - AdjRIBOut provdes a list structure to support route dissemination
 - every peer has its own AdjRIBOut in order to allow each peer to consume Updates at its own pace
 - an optimisation to suppress duplicate updates due to slow consumption can be implnented outside this API
 - using the route identity which is stored with the prefix set
 - the structure is a simple list which holds a set of prefixes
 - prefixes are grouped to maintain packing of prefixes within a single update message
 - fixing up fragmentation of prefixes over common routes is not attempted because it is unlikely to be useful
 - the exception is in the event of route reefersh or peer session initilisation,
 - when an entire route table must be exchanged
-}

import qualified Data.IntMap.Strict
import qualified Data.Tuple

import Common
import Prefixes

type AdjRIBEntry = ( [IPrefix], Int )
type AdjRIBTable = Fifo AdjRIBEntry
type AdjRIBOut = AdjRIBTable
-- TODO I think that the newtype wrapper here is unnecessary!!
-- newtype AdjRIBOut = AdjRIBOut AdjRIBTable
-- fromAdjRIBOut (AdjRIBOut x) = x

newAdjRIBOut = emptyFifo

insertAdjRIBOut :: AdjRIBEntry -> AdjRIBOut -> AdjRIBOut
insertAdjRIBOut are table = ( enqueue table are )

isEmptyAdjRIBOut :: AdjRIBOut -> Bool
isEmptyAdjRIBOut table = nullFifo table

getAdjRIBOut :: AdjRIBOut -> (AdjRIBOut,AdjRIBEntry)
-- undefined on empty
getAdjRIBOut table = ( table' , are ) where (table',are) = dequeue table

getNAdjRIBOut :: Int -> AdjRIBOut -> (AdjRIBOut,AdjRIBEntry)
-- undefined on empty
getNAdjRIBOut n table = ( table' , are ) where (table',are) = dequeue table

getAllAdjRIBOut :: AdjRIBOut -> (AdjRIBOut,[AdjRIBEntry])
getAllAdjRIBOut table = ( table' , ares ) where (table' , ares) = dequeueAll table

peekAllAdjRIBOut :: AdjRIBOut -> [AdjRIBEntry]
peekAllAdjRIBOut table = peekAll table

groomAdjRIBList :: [AdjRIBEntry] -> [AdjRIBEntry]
groomAdjRIBList = map Data.Tuple.swap . Data.IntMap.Strict.toList . Data.IntMap.Strict.fromList . map Data.Tuple.swap 
