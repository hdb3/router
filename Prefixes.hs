{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Prefixes where
-- import GHC.Exts
import Data.Binary
import Data.Hashable
import GHC.Generics(Generic)
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits
import Data.IP
import Data.String(IsString,fromString)

import Common

-- IMPORTANT Note re byte ordering
-- x86 byte ordering is inverted w.r.t. network order,
-- The get/put 16/32/64be primitive
-- operations mask this truth, and at register level big endianness behaviour is observed
-- The 'be' ~ Big Endian refers to the external representation, not the internal one!
-- So for example the type Prefix holds IPv4 addresses as 32 bit words in little endian form
-- internally on x86 but get/putWord32be is required to interwork with network order protocols
-- *** HOWEVER *** ...
-- The 'HostAddress' type from Network.Socket, via Data.IP, is a 32 bit word holding IPv4
-- addresses IN REVERSE ORDER !!!
-- Therefore conversions between our Prefix type and HostAddress require byteSwap32
--
-- representation of prefixes as 64 bit words: this mapping allows prefixes to be treated as Ints where useful

data Prefix = Prefix !(Word8,Word32) deriving (Eq,Generic)
newtype IPrefix = IPrefix Int deriving Eq
toPrefix :: IPrefix -> Prefix
toPrefix (IPrefix w64) = Prefix (fromIntegral $ unsafeShiftR w64 32, fromIntegral $ 0xffffffff .&. w64)
fromPrefix :: Prefix -> IPrefix
fromPrefix (Prefix (!l,!v)) = let l' = fromIntegral l :: Int
                                  v' = fromIntegral v :: Int
                              in IPrefix $! unsafeShiftL l' 32 .|. v'
fromPrefixes = map fromPrefix
toPrefixes = map toPrefix

instance IsString Prefix where
    fromString = read

instance IsString IPrefix where
    fromString = read

instance Read IPrefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(fromPrefix $ fromAddrRange a,s')]

instance Read Prefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ readsPrec 0 s in [(fromAddrRange a,s')]

instance Hashable Prefix
instance Show Prefix where
    show = show.toAddrRange

instance Show IPrefix where
    show = show.toAddrRange.toPrefix

shorten pfxs = if length pfxs < 3 then show pfxs else show [head pfxs] ++ " ..(+" ++ show (length pfxs - 1) ++ ")"

subnet :: Prefix -> Word8
subnet (Prefix (s,_)) = s

ip :: Prefix -> Word32
ip (Prefix (_,i)) = i
 
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)

fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = Prefix (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar

-- binary format for attributes is 1 byte flags, 1 byte type code, 1 or 2 byte length value depending on a flag bit, then payload

{-RFC4271 page 20:
 -
 -          Reachability information is encoded as one or more 2-tuples of
         the form <length, prefix>, whose fields are described below:

                  +---------------------------+
                  |   Length (1 octet)        |
                  +---------------------------+
                  |   Prefix (variable)       |
                  +---------------------------+

         The use and the meaning of these fields are as follows:

         a) Length:

            The Length field indicates the length in bits of the IP
            address prefix.  A length of zero indicates a prefix that
            matches all IP addresses (with prefix, itself, of zero
            octets).

         b) Prefix:

            The Prefix field contains an IP address prefix, followed by
            enough trailing bits to make the end of the field fall on an
            octet boundary.  Note that the value of the trailing bits is
            irrelevant.
-}


instance Binary Prefix where 

    put (Prefix (subnet,ip)) | subnet == 0 = putWord8 0
                             | subnet < 9  = do putWord8 subnet
                                                putWord8 (fromIntegral $ unsafeShiftR ip 24)
                             | subnet < 17 = do putWord8 subnet
                                                putWord16be  (fromIntegral $ unsafeShiftR ip 16)
                             | subnet < 25 = do putWord8 subnet
                                                putWord16be  (fromIntegral $ unsafeShiftR ip 16)
                                                putWord8 (fromIntegral $ unsafeShiftR ip 8)
                             | otherwise   = do putWord8 subnet
                                                putWord32be  ip

    get = label "Prefix" $ do
        subnet <- getWord8
        if subnet == 0
        then return $ Prefix (0,0)
        else if subnet < 9
        then do
            w8 <- getWord8
            let ip = unsafeShiftL (fromIntegral w8 :: Word32) 24
            return $ Prefix (subnet,ip)
        else if subnet < 17
        then do
            w16  <- getWord16be
            let ip = unsafeShiftL (fromIntegral w16  :: Word32) 16
            return $ Prefix (subnet,ip)
        else if subnet < 25
        then do
            w16  <- getWord16be
            w8  <- getWord8
            let ip = unsafeShiftL (fromIntegral w16  :: Word32) 16 .|.
                     unsafeShiftL (fromIntegral w8 :: Word32) 8
            return $ Prefix (subnet,ip)
        else do ip <- getWord32be
                return $ Prefix (subnet,ip)
instance {-# OVERLAPPING #-} Binary [Prefix] where
    put = putn
    get = getn
