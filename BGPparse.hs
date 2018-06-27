
module BGPparse where
import Data.ByteString (ByteString,replicate)
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad(unless,fail)

_BGPOpen = 1
_BGPUpdate = 2
_BGPNotify = 3
_BGPKeepalive = 4
_BGPMarker = Data.ByteString.replicate 16 0xff
_BGPVersion = 4 :: Word8

-- data BGPMessage = BGPOpen BGPOpen | BGPKeepalive | BGPNotify BGPNotify | BGPUpdate BGPUpdate deriving Show

{-
data BGPOpen = BGPOpen { myAutonomousSystem :: Word16
                         , holdTime :: Word16
                         , bgpID :: Word32
                         -- TODO - Optional Parameters 
                         }

data BGPNotify = BGPNotify { errorCode :: Word8
                             , errorSubcode :: Word8
                             , errorData :: ByteString }

data BGPUpdate = BGPUpdate { withdrawnRoutes :: ByteString
                             , pathAttributes :: ByteString
                             , nlri :: ByteString } 

-}

data BGPMessage = BGPOpen { myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: Word32 }
                  | BGPKeepalive
                  | BGPNotify { errorCode :: Word8, errorSubcode :: Word8, errorData :: ByteString }
                  | BGPUpdate { withdrawnRoutes :: ByteString, pathAttributes :: ByteString, nlri :: ByteString }
                    deriving Show

instance Binary BGPMessage where
    put (BGPOpen myAutonomousSystem holdTime bgpID) = do putByteString _BGPMarker
                                                         put (16+2+1+1+2+2+4 :: Word16)
                                                         put (_BGPOpen :: Word8)
                                                         put (_BGPVersion :: Word8)
                                                         put myAutonomousSystem
                                                         put holdTime
                                                         put bgpID

    get = do marker <- getByteString 16
             unless (marker == _BGPMarker) (fail "Bad marker")
             msgLen <- getWord16be
             msgType <- getWord8
             case msgType of _BGPOpen -> do msgVer  <- getWord8
                                            unless (msgVer == _BGPVersion) (fail "Bad version")
                                            myAutonomousSystem <- getWord16be
                                            holdTime <- getWord16be
                                            bgpID <- getWord32be
                                            return $ BGPOpen myAutonomousSystem holdTime bgpID

-- 
-- bgpHeader typeCode | typeCode >= _BGPOpen && typeCode <= _BGPKeepalive =
    
-- unparse :: BGPMessage -> ByteString
-- unparse BGPKeepalive = bgpHeader _BGPKeepalive

-- parse :: ByteString -> Either String BGPMessage
-- parse _ = BGPKeepalive
