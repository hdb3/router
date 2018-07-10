{-# LANGUAGE MultiWayIf #-}
module BGPparse where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad(unless)
import Data.Tuple.Extra
import RFC4271
import Capabilities

_BGPOpen = 1 :: Word8
_BGPUpdate = 2 :: Word8
_BGPNotify = 3 :: Word8
_BGPKeepalive = 4 :: Word8
_BGPVersion = 4 :: Word8

data BGPMessage = BGPOpen { myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: Word32, caps :: [ Capability ] }
                  -- | BGPOpen { myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: Word32, optionalParameters :: B.ByteString }
                  | BGPKeepalive
                  | BGPNotify { code :: EnumNotificationCode, subCode :: EnumNotificationOpenSubcode, caps :: [ Capability ] }
                  | BGPUpdate { withdrawnRoutes :: B.ByteString, pathAttributes :: B.ByteString, nlri :: B.ByteString }
                  | BGPTimeout
                  | BGPError String
                  | BGPEndOfStream
                    deriving (Show,Eq)
isKeepalive :: BGPMessage -> Bool
isKeepalive BGPKeepalive = True
isKeepalive _ = False

isOpen :: BGPMessage -> Bool
isOpen (BGPOpen _ _ _ _) = True
isOpen _ = False

instance Binary BGPMessage where

    put (BGPOpen myAutonomousSystem holdTime bgpID caps) = do putWord8 _BGPOpen
                                                              putWord8 _BGPVersion
                                                              putWord16be myAutonomousSystem
                                                              putWord16be holdTime
                                                              putWord32be bgpID
                                                              let optionalParameters = buildOptionalParameters caps
                                                              putWord8 $ fromIntegral $ B.length optionalParameters
                                                              putByteString optionalParameters

    put (BGPUpdate withdrawnRoutes pathAttributes nlri) = do let withdrawnRoutesLength = fromIntegral $ B.length withdrawnRoutes
                                                                 pathAttributesLength = fromIntegral $ B.length pathAttributes
                                                             putWord8 _BGPUpdate
                                                             putWord16be withdrawnRoutesLength
                                                             putByteString withdrawnRoutes
                                                             putWord16be pathAttributesLength
                                                             putByteString pathAttributes
                                                             putByteString nlri

    put (BGPNotify code subCode caps) = do putWord8 _BGPNotify
                                           putWord8 $ encode8 code
                                           putWord8 $ encode8 subCode
                                           putLazyByteString $ encode caps

    put BGPKeepalive                                = putWord8 _BGPKeepalive

    get = do msgType <- getWord8
             if | _BGPOpen == msgType -> do
                                           msgVer  <- getWord8
                                           unless (msgVer == _BGPVersion) (fail "Bad version(Open)")
                                           myAutonomousSystem <- getWord16be
                                           holdTime <- getWord16be
                                           bgpID <- getWord32be
                                           optionalParametersLength <- getWord8
                                           optionalParameters <- getRemainingLazyByteString
                                           unless (optionalParametersLength == fromIntegral (L.length optionalParameters))
                                                  (fail "optional parameter length wrong (Open)")
                                           return $ BGPOpen myAutonomousSystem holdTime bgpID  ( parseOptionalParameters $ L.toStrict optionalParameters )
                | _BGPUpdate == msgType -> do
                                           withdrawnRoutesLength <- getWord16be
                                           withdrawnRoutes <- getByteString $ fromIntegral withdrawnRoutesLength
                                           pathAttributesLength <- getWord16be
                                           pathAttributes <- getByteString $ fromIntegral pathAttributesLength
                                           nlri <- getRemainingLazyByteString
                                           return $ BGPUpdate withdrawnRoutes pathAttributes $ L.toStrict nlri
                | _BGPNotify == msgType -> do
                                           errorCode <- getWord8
                                           errorSubcode <- getWord8
                                           errorData <- getRemainingLazyByteString
                                           return $ BGPNotify (decode8 errorCode) (decode8 errorSubcode) (decode errorData)
                | _BGPKeepalive == msgType -> return BGPKeepalive
                | otherwise -> fail "Bad type code"
