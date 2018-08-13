{-# LANGUAGE ViewPatterns #-}

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

import Common
import Prefixes
import PathTable

type AdjRIBEntry = ( [IPrefix], RouteId )
type AdjRIBTable = Fifo AdjRIBEntry
newtype AdjRIBOut = AdjRIBOut AdjRIBTable

newAdjRIBOut = AdjRIBOut emptyFifo

insertAdjRIBOut :: AdjRIBOut -> AdjRIBEntry -> AdjRIBOut
insertAdjRIBOut (AdjRIBOut table) are = AdjRIBOut ( enqueue table are )

isEmptyAdjRIBOut :: AdjRIBOut -> Bool
isEmptyAdjRIBOut (AdjRIBOut table) = nullFifo table

simpleGetAdjRIBOut :: AdjRIBOut -> (AdjRIBOut,AdjRIBEntry)
-- undefined on empty
simpleGetAdjRIBOut (AdjRIBOut table) = ( AdjRIBOut  table' , are ) where (table',are) = dequeue table
