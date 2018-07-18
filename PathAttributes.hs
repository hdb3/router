{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
module PathAttributes (module Codes, module PathAttributes, module ASPath) where
import RFC4271
import Codes
import Common
import Data.Binary(Binary(..),encode,decode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.IP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad
import ASPath

data ExtendedCommunities = ExtendedCommunities deriving (Show,Eq)
type LargeCommunity = (Word32,Word32,Word32)

checkForRequiredPathAttributes :: [PathAttribute] -> Bool
checkForRequiredPathAttributes pas = included requiredPathAttributes (map identify pas)
-- checkForRequiredPathAttributes = (included requiredPathAttributes)  . (map identify)

data PathAttribute = PathAttributeOrigin Word8 | -- toDo = make the parameter an enum
                     PathAttributeASPath ASPath2 |
                     PathAttributeNextHop IPv4 |
                     PathAttributeMultiExitDisc Word32 |
                     PathAttributeLocalPref Word32 |
                     PathAttributeAtomicAggregate | -- a null attribute
                     PathAttributeAggregator ( Word16 , IPv4 ) | -- the first parameter is an AS number - in AS4 world is 4 bytes not two.....
                     PathAttributeCommunities [Word32] |
                     PathAttributeMPREachNLRI B.ByteString |
                     PathAttributeMPUnreachNLRI B.ByteString |
                     PathAttributeExtendedCommunities [Word64] |
                     PathAttributeAS4Path ASPath4 |
                     PathAttributeAS4Aggregator (Word32,Word32) |
                     PathAttributeConnector B.ByteString |
                     PathAttributeASPathlimit B.ByteString |
                     PathAttributeLargeCommunity [LargeCommunity] |
                     PathAttributeAttrSet B.ByteString |
                     PathAttributeUnknown B.ByteString
                     deriving (Show,Eq)

-- binary format for attributes is 1 byte flags, 1 byte type code, 1 or 2 byte length value depending on a flag bit, then payload

putAttributeNull :: PathAttributeTypeCode -> Put
putAttributeNull code = do putWord8 (flagsOf code)
                           putWord8 (encode8 code)
                           putWord8 0 -- length of payload

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

putAttributeWords32 :: PathAttributeTypeCode -> [Word32] -> Put
putAttributeWords32 code ws | 63 > length ws = do putWord8 (flagsOf code)
                                                  putWord8 (encode8 code)
                                                  putWord8 $ fromIntegral $ 4 * length ws -- length of payload
                                                  put ws

putAttributeWord64 :: PathAttributeTypeCode -> Word64 -> Put
putAttributeWord64 code v = do putWord8 (flagsOf code)
                               putWord8 (encode8 code)
                               putWord8 8 -- length of payload
                               putWord64be v

putAttributeAggregator :: Word16 -> Word32 -> Put
putAttributeAggregator as bgpid = do putWord8 (flagsOf TypeCodePathAttributeAggregator)
                                     putWord8 (encode8 TypeCodePathAttributeAggregator)
                                     putWord8 6 -- length of payload
                                     putWord16be as
                                     putWord32be bgpid

-- use for attributes upto permitted maximum of 65535 bytes in length
putAttributeByteString :: PathAttributeTypeCode -> L.ByteString -> Put
putAttributeByteString code b = do putWord8 (setExtended $ flagsOf code)
                                   putWord8 (encode8 code)
                                   putWord16be (fromIntegral $ L.length b) -- length of payload
                                   putLazyByteString b

-- use for attributes known to be less than 256 bytes in length
putShortAttributeByteString :: PathAttributeTypeCode -> L.ByteString -> Put
putShortAttributeByteString code b = do putWord8 (flagsOf code)
                                        putWord8 (encode8 code)
                                        putWord8 (fromIntegral $ L.length b) -- length of payload
                                        putLazyByteString b

-- use for attributes not known to be less than 256 bytes in length 
putFlexAttributeByteString :: PathAttributeTypeCode -> L.ByteString -> Put
putFlexAttributeByteString code b | L.length b > 255 = putAttributeByteString code b
                                  | otherwise = putShortAttributeByteString code b

instance Binary PathAttribute where 

    put (PathAttributeOrigin a) = putAttributeWord8 TypeCodePathAttributeOrigin a
    put (PathAttributeNextHop a) = putAttributeWord32 TypeCodePathAttributeNextHop (byteSwap32 $ toHostAddress a)
    put (PathAttributeMultiExitDisc a) = putAttributeWord32 TypeCodePathAttributeMultiExitDisc a
    put (PathAttributeLocalPref a) = putAttributeWord32 TypeCodePathAttributeLocalPref a
    put (PathAttributeASPath a) = putAttributeByteString TypeCodePathAttributeASPath (encode a)
    put (PathAttributeAtomicAggregate) = putAttributeNull TypeCodePathAttributeAtomicAggregate
    put (PathAttributeAggregator (as,bgpid)) = putAttributeAggregator as ( toHostAddress bgpid )
    put (PathAttributeCommunities a) = putAttributeByteString TypeCodePathAttributeCommunities (encode a)
    put (PathAttributeExtendedCommunities a) = putAttributeByteString TypeCodePathAttributeExtendedCommunities (encode a)
    put (PathAttributeAS4Path a) = putAttributeByteString TypeCodePathAttributeAS4Path (encode a)
    put (PathAttributeLargeCommunity a) = putAttributeByteString TypeCodePathAttributeLargeCommunity (encode a)

    get = label "PathAttribute" $ do
             flags <- getWord8
             code'  <- getWord8
             let code = decode8 code'
             len <- if extendedBitTest flags then do l <- getWord16be
                                                     return (fromIntegral l :: Int)  
                                             else do l <- getWord8
                                                     return (fromIntegral l :: Int)  
             unless (flagCheck flags code) (fail "Bad Flags")

             if | TypeCodePathAttributeOrigin == code -> do 
                 unless (len == 1) (fail "Bad Length")
                 v  <- getWord8
                 unless (v < 3) (fail "Bad Origin Code")
                 return $ PathAttributeOrigin v

                | TypeCodePathAttributeASPath == code -> do
                    bs <- getLazyByteString (fromIntegral len)
                    return $ PathAttributeASPath (decode bs)

                | TypeCodePathAttributeNextHop == code -> do
                  v <- getWord32le
                  return $ PathAttributeNextHop (fromHostAddress v)

                | TypeCodePathAttributeMultiExitDisc == code -> do
                  v <- getWord32be
                  return $ PathAttributeMultiExitDisc v

                | TypeCodePathAttributeLocalPref == code -> do
                  v <- getWord32be
                  return $ PathAttributeLocalPref v

                | TypeCodePathAttributeAtomicAggregate == code -> return PathAttributeAtomicAggregate

                | TypeCodePathAttributeAggregator == code -> do
                    as <- getWord16be
                    bgpid <- getWord32le
                    return $ PathAttributeAggregator (as,fromHostAddress bgpid)

                | TypeCodePathAttributeCommunities == code -> do
                    ws <- getMany ( len `div` 4) 
                    return $ PathAttributeCommunities ws

                | TypeCodePathAttributeMPREachNLRI == code -> do
                    bs <- getByteString (fromIntegral len)
                    return $ PathAttributeMPREachNLRI bs

                | TypeCodePathAttributeMPUnreachNLRI == code -> do
                    bs <- getByteString (fromIntegral len)
                    return $ PathAttributeMPUnreachNLRI bs

                | TypeCodePathAttributeExtendedCommunities == code -> do
                    ws <- getMany ( len `div` 8)
                    return $ PathAttributeExtendedCommunities ws

                | TypeCodePathAttributeAS4Path == code -> do
                    bs <- getLazyByteString (fromIntegral len)
                    return $ PathAttributeAS4Path (decode bs)

                | TypeCodePathAttributeAS4Aggregator == code -> do
                    v1 <- getWord32be
                    v2 <- getWord32be
                    return $ PathAttributeAS4Aggregator (v1,v2)

                | TypeCodePathAttributeConnector == code -> do
                    bs <- getByteString (fromIntegral len)
                    return $ PathAttributeConnector bs

                | TypeCodePathAttributeASPathlimit == code -> do
                    bs <- getByteString (fromIntegral len)
                    return $ PathAttributeASPathlimit bs

                | TypeCodePathAttributeLargeCommunity == code -> do
                    ws <- getMany ( len `div` 12)
                    return $ PathAttributeLargeCommunity ws

                | TypeCodePathAttributeAttrSet == code -> do
                    bs <- getByteString (fromIntegral len)
                    return $ PathAttributeAttrSet bs

                | TypeCodePathAttributeUnknown == code -> do
                    bs <- getByteString (fromIntegral len)
                    fail ("unknown type code: " ++ show code')
                    return $ PathAttributeUnknown bs

instance {-# OVERLAPPING #-} Binary [PathAttribute] where

    put = putn
    get = getn

instance {-# OVERLAPPING #-} Binary [Word64] where

    put = putn
    get = getn

instance {-# OVERLAPPING #-} Binary [LargeCommunity] where

    put = putn
    get = getn

identify :: PathAttribute -> PathAttributeTypeCode
identify PathAttributeOrigin{} = TypeCodePathAttributeOrigin
identify PathAttributeASPath{} = TypeCodePathAttributeASPath
identify PathAttributeNextHop{} = TypeCodePathAttributeNextHop
identify PathAttributeMultiExitDisc{} = TypeCodePathAttributeMultiExitDisc
identify PathAttributeLocalPref{} = TypeCodePathAttributeLocalPref
identify PathAttributeAtomicAggregate{} = TypeCodePathAttributeAtomicAggregate
identify PathAttributeAggregator{} = TypeCodePathAttributeAggregator
identify PathAttributeCommunities{} = TypeCodePathAttributeCommunities
identify PathAttributeMPREachNLRI{} = TypeCodePathAttributeMPREachNLRI
identify PathAttributeMPUnreachNLRI{} = TypeCodePathAttributeMPUnreachNLRI
identify PathAttributeExtendedCommunities{} = TypeCodePathAttributeExtendedCommunities
identify PathAttributeAS4Path{} = TypeCodePathAttributeAS4Path
identify PathAttributeAS4Aggregator{} = TypeCodePathAttributeAS4Aggregator
identify PathAttributeConnector{} = TypeCodePathAttributeConnector
identify PathAttributeASPathlimit{} = TypeCodePathAttributeASPathlimit
identify PathAttributeLargeCommunity{} = TypeCodePathAttributeLargeCommunity
identify PathAttributeAttrSet{} = TypeCodePathAttributeAttrSet
identify PathAttributeUnknown{} = TypeCodePathAttributeUnknown
