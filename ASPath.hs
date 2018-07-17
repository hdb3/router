{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
module ASPath where
import RFC4271
import Codes
import Common
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as B
import Control.Monad

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

newtype ASPath = ASPath [ASSegment] deriving (Show,Eq)
data ASSegment = ASSet [ASNumber] | ASSequence [ASNumber] deriving (Show,Eq) 
newtype ASNumber = ASNumber Word16 deriving (Show,Eq,Read)
-- data ASNumber = ASNumber Word16 deriving (Show,Eq,Read)
instance Num ASNumber where
    fromInteger x = ASNumber (fromIntegral x)

instance Binary ASNumber where
    put (ASNumber w) = putWord16le w
    get = do w <- getWord16le
             return (ASNumber w)

instance {-# OVERLAPPING #-} Binary [ASNumber] where
    put = putn
    get = getn

instance {-# OVERLAPPING #-} Binary [ASSegment] where
    put = putn
    get = getn

putASSegmentElement :: ASSegmentElementTypeCode -> [ASNumber] -> Put
putASSegmentElement code asns = do putWord8 (encode8 code)
                                   putWord8 (fromIntegral $ length asns)
                                   put asns
instance Binary ASPath where 
    get = do segments <- get
             return (ASPath segments)

    put (ASPath segments) =  put segments

instance Binary ASSegment where 

    put (ASSet asns) = putASSegmentElement EnumASSet asns
    put (ASSequence asns) = putASSegmentElement EnumASSequence asns

    get = do 
             code'  <- getWord8
             let code = decode8 code'
             len <- getWord8
             asns <- getNasns len
             if | code == EnumASSet -> return $ ASSet asns
                | code == EnumASSequence -> return $ ASSequence asns
                | otherwise -> fail "invalid code in ASpath"
             where
             getNasns :: Word8 -> Get [ASNumber]
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
