module Codes where
import RFC4271

data PathAttributeTypeCode = PathAttributeOrigin | PathAttributeASPath | PathAttributeNextHop | PathAttributeMultiExitDisc | PathAttributeLocalPref |
                             PathAttributeAtomicAggregate | PathAttributeAggregator | PathAttributeCommunities | PathAttributeMPREachNLRI |
                             PathAttributeMPUnreachNLRI | PathAttributeExtendedCommunities | PathAttributeAS4Path | PathAttributeAS4Aggregator |
                             PathAttributeConnector | PathAttributeASPathlimit | PathAttributeLargeCommunity | PathAttributeAttrSet
                            deriving (Show,Eq)

allPathAttributeTypeCodes = [ PathAttributeOrigin , PathAttributeASPath , PathAttributeNextHop , PathAttributeMultiExitDisc , PathAttributeLocalPref ,
                             PathAttributeAtomicAggregate , PathAttributeAggregator , PathAttributeCommunities , PathAttributeMPREachNLRI ,
                             PathAttributeMPUnreachNLRI , PathAttributeExtendedCommunities , PathAttributeAS4Path , PathAttributeAS4Aggregator ,
                             PathAttributeConnector , PathAttributeASPathlimit , PathAttributeLargeCommunity , PathAttributeAttrSet]

instance EnumWord8 PathAttributeTypeCode where
instance Enum PathAttributeTypeCode where

    toEnum n   | n == 1 = PathAttributeOrigin
               | n == 2 = PathAttributeASPath
               | n == 3 = PathAttributeNextHop
               | n == 4 = PathAttributeMultiExitDisc
               | n == 5 = PathAttributeLocalPref
               | n == 6 = PathAttributeAtomicAggregate
               | n == 7 = PathAttributeAggregator
               | n == 8 = PathAttributeCommunities
               | n == 14 = PathAttributeMPREachNLRI
               | n == 15 = PathAttributeMPUnreachNLRI
               | n == 16 = PathAttributeExtendedCommunities
               | n == 17 = PathAttributeAS4Path
               | n == 18 = PathAttributeAS4Aggregator
               | n == 20 = PathAttributeConnector
               | n == 21 = PathAttributeASPathlimit
               | n == 32 = PathAttributeLargeCommunity
               | n == 128 = PathAttributeAttrSet

    fromEnum e | e == PathAttributeOrigin = 1
               | e == PathAttributeASPath = 2
               | e == PathAttributeNextHop = 3
               | e == PathAttributeMultiExitDisc = 4
               | e == PathAttributeLocalPref = 5
               | e == PathAttributeAtomicAggregate = 6
               | e == PathAttributeAggregator = 7
               | e == PathAttributeCommunities = 8
               | e == PathAttributeMPREachNLRI = 14
               | e == PathAttributeMPUnreachNLRI = 15
               | e == PathAttributeExtendedCommunities = 16
               | e == PathAttributeAS4Path = 17
               | e == PathAttributeAS4Aggregator = 18
               | e == PathAttributeConnector = 20
               | e == PathAttributeASPathlimit = 21
               | e == PathAttributeLargeCommunity = 32
               | e == PathAttributeAttrSet = 128
