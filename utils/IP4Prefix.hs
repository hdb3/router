{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IP4Prefix where
import Data.Word
import Data.Bits
import Data.IP
import Data.String(IsString,fromString)
import Prefix

instance Prefix IP4Prefix where
    fromInt = fromIntegral
    toInt = fromIntegral

instance Prefix (AddrRange IPv4) where
    fromInt = toAddrRange . fromIntegral
    toInt = fromIntegral . fromAddrRange

ipFromInt :: Word64 -> Word32
ipFromInt w64 = fromIntegral $ unsafeShiftR w64 32
subnetFromInt :: Word64 -> Word8
subnetFromInt w64 = fromIntegral $ 0xff .&. w64
intFromIpSubnet ip subnet = unsafeShiftL ip 32 .|. subnet
 
toAddrRange :: Word64 -> AddrRange IPv4
toAddrRange w64 = makeAddrRange (fromHostAddress $ ipFromInt w64) (fromIntegral $ subnetFromInt w64)

fromAddrRange :: AddrRange IPv4 -> Word64
fromAddrRange ar = intFromIpSubnet (fromIntegral $ toHostAddress ip) (fromIntegral subnet) where
                   (ip,subnet) = addrRangePair ar
 
newtype IP4Prefix = IP4Prefix Word64 deriving (Eq,Enum,Ord,Num,Real,Integral)

instance Show IP4Prefix where
    show (IP4Prefix x) = show $ toAddrRange x

instance Read IP4Prefix where
    readsPrec _ = readSpfx where
        readSpfx s = let (a,s') = head $ readsPrec 0 s in [(IP4Prefix $ fromAddrRange a,s')]

instance IsString IP4Prefix where
    fromString = read

{-
instance Read Prefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(fromPrefix $ fromAddrRange a,s')]

instance Read Prefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(fromAddrRange a,s')]

instance Show Prefix where
    show = show.toAddrRange

instance Show Prefix where
    show = show.toAddrRange.toPrefix
-}
