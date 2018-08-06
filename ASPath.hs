{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module ASPath where
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.List(foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad

import RFC4271
import Codes
import Common

-- data ASPath = ASPath deriving (Show,Eq)
-- data Aggregator = Aggregator deriving (Show,Eq)
-- data Communities = Communities deriving (Show,Eq)
-- data ExtendedCommunities = ExtendedCommunities deriving (Show,Eq)

-- binary format for AS path is a sequence of AS path segments
-- AS path segments are TLVs, however the 'length' is not a byte count
-- it is the number of included AS numbers
-- the type is 1 or two which codes either a Set or Sequence
-- note: 4 byte AS numbers may be used inthe AS PATH as well as in the AS4_PATH
-- therefore decoding AS_PATH requires to know whether 2 or 4 byte AS numbers are in use.

data ASPath42 = ASPath2 ASPath2| ASPath4 ASPath4 deriving (Show,Eq)
-- data ASPath42 = ASPath2 ( ASPath Word16 ) | ASPath4 (ASPath Word32) deriving (Show,Eq)
type ASPath2 = ASPath Word16
type ASPath4 = ASPath Word32
newtype ASPath asn = ASPath [ASSegment asn] deriving (Show,Eq)
data ASSegment asn = ASSet [asn] | ASSequence [asn] deriving (Show,Eq) 

asPrePend :: ASNumber a => a -> ASPath a -> ASPath a
asPrePend  asn (ASPath segments) = ASPath (asPrePend' asn segments)
asPrePend' asn [] = [ASSequence [asn]]
asPrePend' asn (ASSet sets : segs) = ASSequence [asn] : ASSet sets : segs
asPrePend' asn (ASSequence seqs : segs) | length seqs < 255 = ASSequence (asn:seqs) : segs
                                        | otherwise         = ASSequence [asn] : ASSequence seqs : segs
{-
asPathLength :: ASPath a -> Int
asPathLength ( ASPath asPath) = foldl' addSegLength 0 asPath where
    addSegLength acc (ASSet _ ) = acc + 1
    addSegLength acc (ASSequence ax ) = acc + length ax
-}

asPathLength' :: ASPath a -> Int
asPathLength' ( ASPath asPath) = foldl' addSegLength 0 asPath where
    addSegLength acc (ASSet _ ) = acc + 1
    addSegLength acc (ASSequence ax ) = acc + length ax
asPathLength :: ASPath42 -> Int
asPathLength (ASPath2 asp) = asPathLength' asp
asPathLength (ASPath4 asp) = asPathLength' asp

instance {-# OVERLAPPING #-}(ASNumber asn) =>  Binary [ASSegment asn] where
    put = putn
    get = getn

class (Eq a, Num a, Show a, Read a, Binary a) => ASNumber a where
    putASSegmentElement :: ASSegmentElementTypeCode -> [a] -> Put
    putASSegmentElement code asns = do putWord8 (encode8 code)
                                       putWord8 (fromIntegral $ length asns)
                                       putn asns
instance ASNumber Word16 where
as2list :: Integral a => [a] -> [Word16]
as2list = map fromIntegral

instance ASNumber Word32 where
as4list :: Integral a => [a] -> [Word32]
as4list = map fromIntegral

instance Binary ASPath42 where
    get = undefined
    put (ASPath2 asp) = put asp
    put (ASPath4 asp) = put asp

decodeAsASPath2 :: L.ByteString -> ASPath42
-- decodeAsASPath2 _ = ASPath2 (ASPath [])
decodeAsASPath2 bytes = ASPath2 (decode bytes)

decodeAsASPath4 :: L.ByteString -> ASPath42
decodeAsASPath4 bytes = ASPath4 (decode bytes)
-- decodeAsASPath4 _ = ASPath4 (ASPath [])

instance (ASNumber asn) => Binary (ASPath asn) where 
    get = label "ASPath" $ do
             segments <- get
             return (ASPath segments)

    put (ASPath segments) =  put segments

instance (ASNumber asn) => Binary (ASSegment asn) where 

    put (ASSet asns) = putASSegmentElement EnumASSet asns
    put (ASSequence asns) = putASSegmentElement EnumASSequence asns

    get = label "ASSegment" $ do 
             code'  <- getWord8
             let code = decode8 code'
             len <- getWord8
             asns <- getNasns len
             if | code == EnumASSet -> return $ ASSet asns
                | code == EnumASSequence -> return $ ASSequence asns
                | otherwise -> fail "invalid code in ASpath"
             where
             getNasns :: (Binary asn) => Word8 -> Get [asn]
             getNasns n | n == 0 = return []
                        | otherwise = do asn <- get
                                         asns <- getNasns (n-1)
                                         return (asn:asns)

-- ----------------------------------------
-- candidate for Codes.hs
-- ----------------------------------------
--

data ASSegmentElementTypeCode = EnumASSet | EnumASSequence deriving (Show,Eq)

instance EnumWord8 ASSegmentElementTypeCode where
instance Enum ASSegmentElementTypeCode where

    toEnum n   | n == 1 = EnumASSet
               | n == 2 = EnumASSequence

    fromEnum e | e == EnumASSet = 1
               | e == EnumASSequence = 2
