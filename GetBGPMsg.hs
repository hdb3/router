{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances,BangPatterns #-}
module GetBGPMsg (getBgpMessage,sndBgpMessage) where

{- BGP messages once received are bytestrings - 
 - removing the static marker and length fields is
 - an obvious simplification.   Subsequent parsers
 - need not manage them.  However this means that binary represntations will need to have
 - the marker and length field reapplied before transmission on the network.
-}

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket hiding(send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString.Lazy as L
import Control.Monad(when,unless,fail)

import Common

newtype BGPMessage = BGPMessage L.ByteString

!lBGPMarker = L.replicate 16 0xff
!_BGPMarker = B.replicate 16 0xff
instance Binary BGPMessage where 

    put (BGPMessage bs) = do putLazyByteString lBGPMarker
                             putWord16be (fromIntegral $ L.length bs +18)
                             putLazyByteString bs

    get = label "BGPMessage" $ do
        empty <- isEmpty
        when empty (fail "BGP end of stream")
        marker <- getLazyByteString 16
        unless ( marker == lBGPMarker ) (fail "BGP marker synchronisation error")
        len <- getWord16be
        bs <- getLazyByteString (fromIntegral len)
        return (BGPMessage bs)


instance {-# OVERLAPPING #-} Binary [BGPMessage] where
    put = putn
    get = getn

sndBgpMessage :: Socket -> L.ByteString -> IO ()
sndBgpMessage sock bgpMsg = L.sendAll sock $ encode (BGPMessage bgpMsg)
sndBgpMessage' :: Socket -> BGPMessage -> IO ()
sndBgpMessage' sock bgpMsg = L.sendAll sock (encode bgpMsg)

getBgpMessage' :: Socket -> IO BGPMessage
getBgpMessage' sock = do bgpMsg <- getBgpMessage sock
                         return $ BGPMessage bgpMsg

getBgpMessage :: Socket -> IO L.ByteString
getBgpMessage sock = do -- putStrLn "getBgpMessage"
                        msgHdr <- recv' sock 18
                        let (marker,lenW) = L.splitAt 16 msgHdr
                            l0 = fromIntegral $ L.index lenW 0 :: Word16
                            l1 = fromIntegral $ L.index lenW 1 :: Word16
                            len = l1 .|. unsafeShiftL l0 8
                            bodyLength = fromIntegral len - 18
                        -- putStrLn $ "payload length: " ++ show bodyLength
                        unless (marker == lBGPMarker)
                               (fail "Bad marker")
                        body <- recv' sock bodyLength
                        unless (bodyLength == L.length body)
                                (fail $ "internal error - short read" ++ " - asked " ++ show bodyLength ++ " got " ++ show (L.length body))
                        return body

recv' sock n = get L.empty 0 where
    get buf got | got == n = return buf
                | otherwise = do more <- L.recv sock (n-got)
                                 get (buf `L.append` more) (got + L.length more) 
                      
                        -- return $ L.fromStrict body

-- recv' :: Socket -> Int -> IO L.ByteString
-- recv' :: Socket -> IO L.ByteString
-- recv' sock = L.recv sock 4096 `L.append` recv' sock

