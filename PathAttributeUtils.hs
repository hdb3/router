{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
module PathAttributeUtils where
import Data.Word
import Data.IP
import Data.Maybe(fromJust,fromMaybe)

import Common
import PathAttributes

prePendAS :: ASNumber a => a -> [PathAttribute] -> [PathAttribute]
prePendAS asn = updatePathAttribute TypeCodePathAttributeASPath (asPrePend' asn) where
    asPrePend' asn ( PathAttributeASPath p) = PathAttributeASPath (asPrePend asn p)

getASPathLength :: [PathAttribute] -> Int
getASPathLength pas = maybe
                      0
                      (\(PathAttributeASPath asPath) -> asPathLength asPath)
                      (getPathAttribute TypeCodePathAttributeASPath pas)

getAS2Path = fromJust . getPathAttribute TypeCodePathAttributeASPath
getAS4Path = fromJust . getPathAttribute TypeCodePathAttributeAS4Path

getASPath pax = fromMaybe (getAS2Path pax) (getPathAttribute TypeCodePathAttributeAS4Path pax)

setLocalPref :: Word32 -> [PathAttribute] -> [PathAttribute]
setLocalPref = insertPathAttribute . PathAttributeLocalPref

getLocalPref :: [PathAttribute] -> Word32
getLocalPref pas = maybe 0 (\(PathAttributeLocalPref x) -> x) (getPathAttribute TypeCodePathAttributeLocalPref pas)

setMED :: Word32 -> [PathAttribute] -> [PathAttribute]
setMED = insertPathAttribute . PathAttributeMultiExitDisc

getMED :: [PathAttribute] -> Word32
getMED pas = maybe 0 (\(PathAttributeMultiExitDisc x) -> x) (getPathAttribute TypeCodePathAttributeMultiExitDisc pas)

setOrigin :: Word8 -> [PathAttribute] -> [PathAttribute]
setOrigin = insertPathAttribute . PathAttributeOrigin

getOrigin :: [PathAttribute] -> Word8
getOrigin pas = maybe 0 (\(PathAttributeOrigin x) -> x ) (getPathAttribute TypeCodePathAttributeOrigin pas)

setNextHop :: IPv4 -> [PathAttribute] -> [PathAttribute]
setNextHop = insertPathAttribute . PathAttributeNextHop

getNextHop :: [PathAttribute] -> IPv4
getNextHop pas = maybe "0.0.0.0" (\(PathAttributeNextHop x) -> x ) (getPathAttribute TypeCodePathAttributeNextHop pas)

checkForRequiredPathAttributes :: [PathAttribute] -> Bool
checkForRequiredPathAttributes pas = included requiredPathAttributes (map identify pas)

