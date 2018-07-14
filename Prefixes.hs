{-# LANGUAGE MultiWayIf,FlexibleInstances,OverloadedStrings #-}
module Prefixes where
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits
import Data.IP

newtype Prefix = Prefix (Word8,Word32) deriving (Show,Eq)

subnet :: Prefix -> Word8
subnet (Prefix (s,_)) = s

ip :: Prefix -> Word32
ip (Prefix (_,i)) = i

canonicalPrefix :: Prefix -> Prefix
canonicalPrefix (Prefix (subnet,ip)) | subnet < 33 = Prefix (subnet,canonicalise ip) where
    canonicalise = ((flip unsafeShiftL) (fromIntegral subnet)) . ((flip unsafeShiftR) (fromIntegral subnet))
 
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress ip) (fromIntegral subnet)

fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = Prefix (fromIntegral subnet,toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar

-- data PrefixList = PrefixList [Prefix]
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
                             | subnet < 8  = do putWord8 subnet
                                                putWord8 (fromIntegral $ unsafeShiftR ip 24)
                             | subnet < 16 = do putWord8 subnet
                                                putWord16be (fromIntegral $ unsafeShiftR ip 16)
                             | subnet < 24 = do putWord8 subnet
                                                putWord16be (fromIntegral $ unsafeShiftR ip 16)
                                                putWord8 (fromIntegral $ unsafeShiftR ip 8)
                             | otherwise   = do putWord8 subnet
                                                putWord32be ip

    get = do
        subnet <- getWord8
        if subnet == 0
        then return $ Prefix (0,0)
        else if subnet < 8
        then do
            w8 <- getWord8
            let ip = unsafeShiftL (fromIntegral w8 :: Word32) 24
            return $ Prefix (subnet,ip)
        else if subnet < 16
        then do
            w16be <- getWord16be
            let ip = unsafeShiftL (fromIntegral w16be :: Word32) 16
            return $ Prefix (subnet,ip)
        else if subnet < 24
        then do
            w16be <- getWord16be
            w8  <- getWord8
            let ip = unsafeShiftL (fromIntegral w16be :: Word32) 16 .|.
                     unsafeShiftL (fromIntegral w8 :: Word32) 8
            return $ Prefix (subnet,ip)
        else do ip <- getWord32be
                return $ Prefix (subnet,ip)


{-
putAttr :: PathAttribute -> Put
putAttr = put
getAttr :: Get PathAttribute
getAttr = get
instance {-# OVERLAPPING #-} Binary [PathAttribute] where

    put attrs | null attrs =  return ()
              | otherwise =  do putAttr (head attrs)
                                put ( tail attrs)

    get = getAttrs where
        getAttrs = do
          empty <- isEmpty
          if empty
            then return []
            else do attr <- getAttr
                    attrs <- getAttrs
                    return (attr:attrs)
-}
