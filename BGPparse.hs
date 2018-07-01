{-# LANGUAGE MultiWayIf #-}
module BGPparse where
import qualified Data.ByteString as B
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad(unless,fail)
import qualified Data.ByteString.Lazy as L

_BGPOpen = 1 :: Word8
_BGPUpdate = 2 :: Word8
_BGPNotify = 3 :: Word8
_BGPKeepalive = 4 :: Word8
_BGPMarker = B.replicate 16 0xff
_BGPVersion = 4 :: Word8

data RawBGPMessage = RawBGPMessage { typeCode :: Word8, body :: B.ByteString } deriving (Show,Eq)
instance Binary RawBGPMessage where

    put (RawBGPMessage typeCode body) = do putByteString _BGPMarker
                                           let msgLength = fromIntegral $ B.length body
                                           putWord16be $ 16+2+1+msgLength
                                           putWord8 typeCode
                                           putByteString body

    get = do marker <- getByteString 16
             unless (marker == _BGPMarker) (fail "Bad marker")
             msgLen <- getWord16be
             typeCode <- getWord8
             body <- getRemainingLazyByteString
             return $ RawBGPMessage typeCode $ L.toStrict body

data BGPOpen = BGPOpen { myAutonomousSystem :: Word16,
                         holdTime :: Word16,
                         bgpID :: Word32,
                         optionalParameters :: B.ByteString } deriving (Show,Eq)
instance Binary BGPOpen where

    put (BGPOpen myAutonomousSystem holdTime bgpID optionalParameters) = do
        let optionalParametersLength = fromIntegral $ B.length optionalParameters
        putWord8 _BGPVersion
        putWord16be myAutonomousSystem
        putWord16be holdTime
        putWord32be bgpID
        putWord8 $ fromIntegral optionalParametersLength
        putByteString optionalParameters

    get = do msgVer  <- getWord8
             unless (msgVer == _BGPVersion) (fail "Bad version(Open)")
             myAutonomousSystem <- getWord16be
             holdTime <- getWord16be
             bgpID <- getWord32be
             optionalParametersLength <- getWord8
             optionalParameters <- getRemainingLazyByteString
             unless (optionalParametersLength == (fromIntegral $ L.length optionalParameters))
                    (fail "optional parameter length wrong (Open)")
             return $ BGPOpen myAutonomousSystem holdTime bgpID $ L.toStrict optionalParameters

data BGPUpdate = BGPUpdate { withdrawnRoutes :: B.ByteString,
                             pathAttributes :: B.ByteString,
                             nlri :: B.ByteString } deriving (Show,Eq)

instance Binary BGPUpdate where
    put (BGPUpdate withdrawnRoutes pathAttributes nlri) = do
        let withdrawnRoutesLength = fromIntegral $ B.length withdrawnRoutes
        let pathAttributesLength = fromIntegral $ B.length pathAttributes
        let nlriLength = fromIntegral $ B.length nlri
        putWord16be withdrawnRoutesLength
        putByteString withdrawnRoutes
        putWord16be pathAttributesLength
        putByteString pathAttributes
        putByteString nlri

    get = do withdrawnRoutesLength <- getWord16be
             withdrawnRoutes <- getByteString $ fromIntegral withdrawnRoutesLength
             pathAttributesLength <- getWord16be
             pathAttributes <- getByteString $ fromIntegral pathAttributesLength
             nlri <- getRemainingLazyByteString
             return $ BGPUpdate withdrawnRoutes pathAttributes $ L.toStrict nlri

data BGPNotify = BGPNotify { errorCode :: Word8,
                             errorSubcode :: Word8,
                             errorData :: B.ByteString } deriving (Show,Eq)

instance Binary BGPNotify where
    put (BGPNotify errorCode errorSubcode errorData) = do
        let errorDataLength = fromIntegral $ B.length errorData
        putWord8 errorCode
        putWord8 errorSubcode
        putByteString errorData

    get = do errorCode <- getWord8
             errorSubcode <- getWord8
             errorData <- getRemainingLazyByteString
             return $ BGPNotify errorCode errorSubcode $ L.toStrict errorData

data BGPMessage = Open BGPOpen | Update BGPUpdate | Notify BGPNotify | Keepalive BGPKeepalive
data BGPKeepalive = BGPKeepalive deriving Show
instance Binary BGPMessage where
    put (Keepalive BGPKeepalive) = putByteString $ L.toStrict $ encode (RawBGPMessage _BGPKeepalive (B.empty))

    put (Open msg@(BGPOpen myAutonomousSystem holdTime bgpID optionalParameters)) =
        putByteString $ L.toStrict $ encode (RawBGPMessage _BGPOpen (L.toStrict $ encode msg))

    put (Update msg@(BGPUpdate withdrawnRoutes pathAttributes nlri)) =
        putByteString $ L.toStrict $ encode (RawBGPMessage _BGPUpdate (L.toStrict $ encode msg))

    put (Notify msg@(BGPNotify errorCode errorSubcode errorData)) =
        putByteString $ L.toStrict $ encode (RawBGPMessage _BGPNotify (L.toStrict $ encode msg))

    get = do rawMsg <- getRemainingLazyByteString
             let (RawBGPMessage typeCode body') = decode rawMsg :: RawBGPMessage
                 body = L.fromStrict body'
             if | _BGPOpen == typeCode   -> return $ Open (decode body)
                | _BGPUpdate == typeCode -> return $ Update (decode body)
                | _BGPNotify == typeCode -> return $ Notify (decode body)
                | _BGPKeepalive == typeCode -> return $ Keepalive BGPKeepalive
                | otherwise -> fail "Bad type code"
