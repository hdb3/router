{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
module Capabilities where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.ByteString(ByteString)
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Builder
import Data.Monoid((<>))
import Data.List(find)
import GetTLVs
import RFC4271
import Common

--
-- ref https://www.iana.org/assignments/capability-codes/capability-codes.xml
{-

Value 	Description 	Reference 
1	Multiprotocol Extensions for BGP-4	[RFC2858]
7	BGPsec Capability	[RFC8205]
64	Graceful Restart Capability	[RFC4724]
65	Support for 4-octet AS number capability	[RFC6793]

-}
_CapCodeMultiprotocol = 1
_CapCodeGracefulRestart = 64
_CapCodeAS4 = 65
-- in the python library I have support for the followin optional capabilities
-- multiprotocol (IPv6)
-- gracefull restart
-- AS4 (32-bit ASNs)
{-
 The Capability Value field is defined as:

                     0       7      15      23      31
                     +-------+-------+-------+-------+
                     |      AFI      | Res.  | SAFI  |
                     +-------+-------+-------+-------+

   The use and meaning of this field is as follow:

      AFI  - Address Family Identifier (16 bit), encoded the same way as
          in the Multiprotocol Extensions

      Res. - Reserved (8 bit) field.  SHOULD be set to 0 by the sender
          and ignored by the receiver.

          Note that not setting the field value to 0 may create issues
          for a receiver not ignoring the field.  In addition, this
          definition is problematic if it is ever attempted to redefine
          the field.

      SAFI - Subsequent Address Family Identifier (8 bit), encoded the
          same way as in the Multiprotocol Extensions.

-}

-- graceful restart
-- see RFC 4724
--
--this is a complex capability in theory however the simple instance is very simple
-- we only implement the basic verion
--
--AS4 - 32bit ASNs
--see RFC6793
--the capability is just the local 32bit ASN
--
data Capability = CapMultiprotocol Word16 Word8 
                | CapGracefulRestart Bool Word16
                | CapAS4 Word32 deriving (Show,Eq)

eq_ :: Capability -> Capability -> Bool
eq_ (CapMultiprotocol _ _) (CapMultiprotocol _ _) = True
eq_ (CapGracefulRestart _ _) (CapGracefulRestart _ _) = True
eq_ (CapAS4 _) (CapAS4 _) = True
eq_ _ _ = False

putCap :: Capability -> Put
putCap = put
getCap :: Get Capability
getCap = get

instance {-# OVERLAPPING #-} Binary [Capability] where

    put = putn
    get = getn

instance Binary Capability where

    put (CapAS4 as4) = do
        putWord8 _CapCodeAS4
        putWord8 4
        putWord32be as4

    put (CapGracefulRestart rFlag restartTime) = do
        putWord8 _CapCodeGracefulRestart
        putWord8 2
        putWord16be $ if rFlag then setBit restartTime 15 else restartTime

    put (CapMultiprotocol afi safi) = do
        putWord8 _CapCodeMultiprotocol
        putWord8 4
        putWord16be afi
        putWord8 0
        putWord8 safi

    get = do
        t <- getWord8
        l <- getWord8
        if | t == _CapCodeMultiprotocol -> do
                      afi <- getWord16be
                      _ <- getWord8
                      safi <- getWord8
                      return (CapMultiprotocol afi safi)
           | t == _CapCodeGracefulRestart -> do
                      word0 <- getWord16be
                      let rFlag = testBit word0 15
                          restartTime = word0 .&. 0x0fff
                      return (CapGracefulRestart rFlag restartTime)
           | t == _CapCodeAS4 -> do as <- getWord32be
                                    return $ CapAS4 as -- surely not the most elegant way to say this!!!!
           | otherwise        -> do error $ "Unexpected type code: " ++ show t
                                    return undefined

buildOptionalParameters :: [ Capability ] -> ByteString
buildOptionalParameters capabilities | not $ null capabilities = let caps = L.concat $ map encode capabilities in
                                                                 L.toStrict $ toLazyByteString $ word8 2 <>  word8 (fromIntegral $ L.length caps) <> lazyByteString caps
                                     | otherwise = B.empty

parseOptionalParameters :: ByteString -> [ Capability ]

parseOptionalParameters parametersBS = maybe
                                       []
                                       (map (decode . L.fromStrict) . getTLVs . B.drop 2)
                                       (find (\bs -> 2 == B.index bs 0) (getTLVs parametersBS))
