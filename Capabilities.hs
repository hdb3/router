{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}
{-- LANGUAGE KindSignatures #-}
module Capabilities where
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as B
import Data.ByteString(ByteString)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
-- import Control.Monad(unless)

-- in the python library I have support for the followin optional capabilities
-- multiprotocol
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


class Tupl a where
    put :: a -> (Word8,ByteString)
    get :: (Word8,ByteString) -> a
 
instance Tupl Capability where

    put (CapMultiprotocol afi safi) = (1,encode (CapMultiprotocol' afi safi))
    put (CapGracefulRestart rFlag restartTime) = (64,encode (CapGracefulRestart' rFlag restartTime))
    put (CapAS4 as4) = (65,encode (CapAS4' as4))

    get (t,v) = if | t == 1  -> decode v :: CapMultiprotocol'  
                   | t == 64 -> decode v :: CapGracefulRestart'
                   | t == 65 -> decode v :: CapAS4'

data CapAS4' = CapAS4' Word32 deriving (Show,Eq)
instance Binary CapAS4' where
    put cap@(CapAS4' as4) = putWord32be as4
    get = CapAS4' getWord32be

data CapGracefulRestart' = CapGracefulRestart' Bool Word16
instance Binary CapGracefulRestart' where
    put cap@(CapGracefulRestart' rFlag restartTime) = putWord16be $ if rFlag then setBit restartTime 15 else restartTime
    get = do
        word0 <- getWord16be
        let rFlag = testBit 15 word0
            restartTime = word0 .&. 0x0fff
        return (CapGracefulRestart' rFlag restartTime)

data CapMultiprotocol' = CapMultiprotocol' Word16 Word8 deriving (Show,Eq)
instance Binary CapMultiprotocol' where
    put cap@(CapMultiprotocol afi safi) = do
        putWord16be afi
        putWord8 0
        putWord8 safi
    get = do
        afi <- getWord16be
        _ <- getWord8
        safi <- getWord8
        return (CapMultiprotocol afi safi)
