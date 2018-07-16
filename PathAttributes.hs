{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
module PathAttributes (module Codes, module PathAttributes, module ASPath) where
import RFC4271
import Codes
import Common
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as B
import Control.Monad
import ASPath

-- data ASPath = ASPath deriving (Show,Eq)
data Aggregator = Aggregator deriving (Show,Eq)
data Communities = Communities deriving (Show,Eq)
data ExtendedCommunities = ExtendedCommunities deriving (Show,Eq)

data PathAttribute = PathAttributeOrigin Word8 | -- toDo = make the parameter an enum
                     PathAttributeASPath ASPath |
                     PathAttributeNextHop Word32 | -- should be IPv4
                     PathAttributeMultiExitDisc Word32 |
                     PathAttributeLocalPref Word32 |
                     PathAttributeAtomicAggregate |
                     PathAttributeAggregator Aggregator|
                     PathAttributeCommunities Communities|
                     PathAttributeMPREachNLRI B.ByteString |
                     PathAttributeMPUnreachNLRI B.ByteString |
                     PathAttributeExtendedCommunities ExtendedCommunities|
                     PathAttributeAS4Path ASPath |
                     PathAttributeAS4Aggregator (Word32,Word32) |
                     PathAttributeConnector |
                     PathAttributeASPathlimit |
                     PathAttributeLargeCommunity |
                     PathAttributeAttrSet
                     deriving (Show,Eq)

-- binary format for attributes is 1 byte flags, 1 byte type code, 1 or 2 byte length value depending on a flag bit, then payload

putAttributeWord8 :: PathAttributeTypeCode -> Word8 -> Put
putAttributeWord8 code v = do putWord8 (flagsOf code)
                              putWord8 (encode8 code)
                              putWord8 1 -- length of payload
                              putWord8 v

putAttributeWord32 :: PathAttributeTypeCode -> Word32 -> Put
putAttributeWord32 code v = do putWord8 (flagsOf code)
                               putWord8 (encode8 code)
                               putWord8 4 -- length of payload
                               putWord32be v

instance Binary PathAttribute where 

    put (PathAttributeOrigin a) = putAttributeWord8 TypeCodePathAttributeOrigin a
    put (PathAttributeNextHop a) = putAttributeWord32 TypeCodePathAttributeNextHop a
    put (PathAttributeMultiExitDisc a) = putAttributeWord32 TypeCodePathAttributeMultiExitDisc a
    put (PathAttributeLocalPref a) = putAttributeWord32 TypeCodePathAttributeLocalPref a

    get = do flags <- getWord8
             code'  <- getWord8
             let code = decode8 code'
             len <- if extendedBitTest flags then do l <- getWord16be
                                                     return $ (fromIntegral l :: Int)  
                                             else do l <- getWord8
                                                     return $ (fromIntegral l :: Int)  
             unless (flagCheck flags code) (fail "Bad Flags")

             if | TypeCodePathAttributeOrigin == code -> do 
                 unless (len == 1) (fail "Bad Length")
                 v  <- getWord8
                 unless (v < 3) (fail "Bad Origin Code")
                 return $ PathAttributeOrigin v

                | TypeCodePathAttributeASPath == code -> return undefined

                | TypeCodePathAttributeNextHop == code -> do
                  v <- getWord32be
                  return $ PathAttributeNextHop v

                | TypeCodePathAttributeMultiExitDisc == code -> do
                  v <- getWord32be
                  return $ PathAttributeMultiExitDisc v

                | TypeCodePathAttributeLocalPref == code -> do
                  v <- getWord32be
                  return $ PathAttributeLocalPref v

                | TypeCodePathAttributeAtomicAggregate == code -> return undefined

                | TypeCodePathAttributeAggregator == code -> return undefined

                | TypeCodePathAttributeCommunities == code -> return undefined

                | TypeCodePathAttributeMPREachNLRI == code -> return undefined

                | TypeCodePathAttributeMPUnreachNLRI == code -> return undefined

                | TypeCodePathAttributeExtendedCommunities == code -> return undefined

                | TypeCodePathAttributeAS4Path == code -> return undefined

                | TypeCodePathAttributeAS4Aggregator == code -> do
                    v1 <- getWord32be
                    v2 <- getWord32be
                    return $ PathAttributeAS4Aggregator (v1,v2)

                | TypeCodePathAttributeConnector == code -> return undefined

                | TypeCodePathAttributeASPathlimit == code -> return undefined

                | TypeCodePathAttributeLargeCommunity == code -> return undefined

                | TypeCodePathAttributeAttrSet == code -> return undefined

instance {-# OVERLAPPING #-} Binary [PathAttribute] where

    put = putn
    get = getn
