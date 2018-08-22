module AdjRIBOut where

{-
 - AdjRIBTable provdes a list structure to support route dissemination
 - every peer has its own AdjRIBTable in order to allow each peer to consume Updates at its own pace
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

newAdjRIBTable = emptyFifo

insertAdjRIBTable :: AdjRIBEntry -> AdjRIBTable -> AdjRIBTable
insertAdjRIBTable are table = ( enqueue table are )

isEmptyAdjRIBTable :: AdjRIBTable -> Bool
isEmptyAdjRIBTable table = nullFifo table

getAdjRIBTable :: AdjRIBTable -> (AdjRIBTable,AdjRIBEntry)
-- undefined on empty
getAdjRIBTable table = ( table' , are ) where (table',are) = dequeue table

getNAdjRIBTable :: Int -> AdjRIBTable -> (AdjRIBTable,AdjRIBEntry)
-- undefined on empty
getNAdjRIBTable n table = ( table' , are ) where (table',are) = dequeue table

getAllAdjRIBTable :: AdjRIBTable -> (AdjRIBTable,[AdjRIBEntry])
getAllAdjRIBTable table = ( table' , ares ) where (table' , ares) = dequeueAll table

peekAllAdjRIBTable :: AdjRIBTable -> [AdjRIBEntry]
peekAllAdjRIBTable table = peekAll table

groomAdjRIBList :: [AdjRIBEntry] -> [AdjRIBEntry]
groomAdjRIBList = map Data.Tuple.swap . Data.IntMap.Strict.toList . Data.IntMap.Strict.fromList . map Data.Tuple.swap 
