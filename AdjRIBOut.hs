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

import qualified Data.Sequence as Seq

import Prefixes
import PathTable

-- **** TODO - the filter needs to be applied to A LIST of prefixes, and use the output list, unless empty!!!!
type AdjRIBOutFilter = Prefix -> PathTableEntry -> Bool
data AdjRIBOut = AdjRIBOut { table :: Seq.Seq AdjRIBEntry, filter :: AdjRIBOutFilter }
data AdjRIBEntry = AdjRIBEntry { prefixes :: [IPrefix], route :: RouteId }
newAdjRIBOut = AdjRIBOut Seq.empty (\_ _ -> False)
insertAdjRIBOut :: AdjRIBOut -> [Prefix] -> PathTableEntry -> AdjRIBOut
insertAdjRIBOut aro@(AdjRIBOut table filter) prefixes pte = if filter prefixes pte then aro else (AdjRIBEntry (fromPrefixes prefixes)  Seq.:< table) filter))
-- insertAdjRIBOut aro@(AdjRIBOut table filter) prefixes pte = if filter prefixes pte then aro else (AdjRIBOut (fromPrefixes prefixes Seq.:< (Seq.viewl table) filter))
isEmptyAdjRIBOut :: AdjRIBOut -> Bool
isEmptyAdjRIBOut (AdjRIBOut table _) = Seq.null table

simpleGetAdjRIBOut :: AdjRIBOut -> (AdjRIBOut,Maybe AdjRIBEntry)
simpleGetAdjRIBOut aro@(AdjRIBOut table filter) | Seq.null table = (aro,Nothing)
                                                | otherwise = (aro', Just are) where
                                                  (table' Seq.:> are) = Seq.viewr table
                                                  aro' = (AdjRIBOut table' filter)
