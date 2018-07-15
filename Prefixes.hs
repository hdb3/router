{- LANGUAGE MultiWayIf,FlexibleInstances,OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Prefixes where
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits
import Data.IP

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

newtype Prefix = Prefix (Word8,Word32) deriving (Eq)
instance Show Prefix where
    show = show.toAddrRange

subnet :: Prefix -> Word8
subnet (Prefix (s,_)) = s

ip :: Prefix -> Word32
ip (Prefix (_,i)) = i

canonicalPrefix :: Prefix -> Prefix
canonicalPrefix (Prefix (subnet,ip)) | subnet < 33 = Prefix (subnet,canonicalise ip) where
    canonicalise = (`unsafeShiftR` (fromIntegral $ 32 - subnet)) .
                   (`unsafeShiftL` (fromIntegral $ 32 - subnet))
 
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)

fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = Prefix (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar

newtype PrefixList = PrefixList [Prefix] deriving (Show,Eq)

-- binary format for attributes is 1 byte flags, 1 byte type code, 1 or 2 byte length value depending on a flag bit, then payload

-- instance {-# OVERLAPPING #-} Binary [PrefixList] where

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

    get = do
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


putPrefix :: Prefix -> Put
putPrefix = put
getPrefix :: Get Prefix
getPrefix = get
instance {-# OVERLAPPING #-} Binary [Prefix] where

    put pfxs | null pfxs =  return ()
              | otherwise =  do putPrefix (head pfxs)
                                put ( tail pfxs)

    get = getPrefixes where
        getPrefixes = do
          empty <- isEmpty
          if empty
            then return []
            else do pfx <- getPrefix
                    pfxs <- getPrefixes
                    return (pfx:pfxs)
{-
-}

{-
class BinList b => Binary b where
    decodeL :: [b] -> L.ByteString
    decode8 = toEnum . fromIntegral
    encode8 :: e -> Word8
    encode8 = fromIntegral . fromEnum

-}
