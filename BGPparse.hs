
module BGPparse where
--import Data.ByteString (ByteString,replicate)
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

data BGPMessage = BGPOpen { myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: Word32, optionalParameters :: B.ByteString }
                  | BGPKeepalive
                  | BGPNotify { errorCode :: Word8, errorSubcode :: Word8, errorData :: B.ByteString }
                  | BGPUpdate { withdrawnRoutes :: B.ByteString, pathAttributes :: B.ByteString, nlri :: B.ByteString }
                    deriving (Show,Eq)

instance Binary BGPMessage where

    put (BGPOpen myAutonomousSystem holdTime bgpID optionalParameters) = do let optionalParametersLength = fromIntegral $ B.length optionalParameters
                                                                            putByteString _BGPMarker
                                                                            putWord16be $ 16+2+1+1+2+2+4+1+optionalParametersLength
                                                                            putWord8 _BGPOpen
                                                                            putWord8 _BGPVersion
                                                                            putWord16be myAutonomousSystem
                                                                            putWord16be holdTime
                                                                            putWord32be bgpID
                                                                            putWord8 $ fromIntegral optionalParametersLength
                                                                            putByteString optionalParameters

    put (BGPUpdate withdrawnRoutes pathAttributes nlri) = do let withdrawnRoutesLength = fromIntegral $ B.length withdrawnRoutes
                                                                 pathAttributesLength = fromIntegral $ B.length pathAttributes
                                                                 nlriLength = fromIntegral $ B.length nlri
                                                             putByteString _BGPMarker
                                                             putWord16be $ 16+2+1+2+withdrawnRoutesLength+2+pathAttributesLength+nlriLength
                                                             putWord8 _BGPUpdate
                                                             putWord16be withdrawnRoutesLength
                                                             putByteString withdrawnRoutes
                                                             putWord16be pathAttributesLength
                                                             putByteString pathAttributes
                                                             putByteString nlri

    put (BGPNotify errorCode errorSubcode errorData) = do let errorDataLength = fromIntegral $ B.length errorData
                                                          putByteString _BGPMarker
                                                          putWord16be $ 16+2+1+errorDataLength
                                                          putWord8 _BGPNotify
                                                          putWord8 errorCode
                                                          putWord8 errorSubcode
                                                          putByteString errorData

    put BGPKeepalive                                = do putByteString _BGPMarker
                                                         putWord16be $ 16+2+1
                                                         putWord8 _BGPKeepalive

    get = do marker <- getByteString 16
             unless (marker == _BGPMarker) (fail "Bad marker")
             msgLen <- getWord16be
             msgType <- getWord8
             case msgType of
                             1 -> do msgVer  <- getWord8
                                     unless (msgVer == _BGPVersion) (fail "Bad version(Open)")
                                     myAutonomousSystem <- getWord16be
                                     holdTime <- getWord16be
                                     bgpID <- getWord32be
                                     optionalParametersLength <- getWord8
                                     optionalParameters <- getRemainingLazyByteString
                                     unless (optionalParametersLength == (fromIntegral $ L.length optionalParameters))
                                            (fail "optional parameter length wrong (Open)")
                                     return $ BGPOpen myAutonomousSystem holdTime bgpID $ L.toStrict optionalParameters
                             2   -> do withdrawnRoutesLength <- getWord16be
                                       withdrawnRoutes <- getByteString $ fromIntegral withdrawnRoutesLength
                                       pathAttributesLength <- getWord16be
                                       pathAttributes <- getByteString $ fromIntegral pathAttributesLength
                                       nlri <- getRemainingLazyByteString
                                       return $ BGPUpdate withdrawnRoutes pathAttributes $ L.toStrict nlri
                             3   -> do errorCode <- getWord8
                                       errorSubcode <- getWord8
                                       errorData <- getRemainingLazyByteString
                                       return $ BGPNotify errorCode errorSubcode $ L.toStrict errorData
                             4 -> do unless (msgLen == 16+2+1) (fail "Bad length (Keepalive)")
                                     return BGPKeepalive
                             _ -> do fail "Bad type code"
