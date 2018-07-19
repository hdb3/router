{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances,BangPatterns #-}
module GetBGPMsg where
-- module GetBGPMsg (BufferedSocket(..),getBgpMessage,sndBgpMessage) where

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
import Data.Either(isLeft)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket(Socket)
import qualified Network.Socket.ByteString.Lazy as L
import Control.Monad(when,unless,fail)

import Common

newtype BGPByteString = BGPByteString L.ByteString

data BufferedSocket = BufferedSocket Socket L.ByteString (Either String BGPByteString)
newBufferedSocket sock = BufferedSocket sock L.empty (Right (BGPByteString L.empty))
socket (BufferedSocket s _ _) = s
buf (BufferedSocket _ b _) = b
result (BufferedSocket _ _ res) = res
getNext :: BufferedSocket -> IO BufferedSocket
getNext (BufferedSocket sock buffer result)
                                          | isLeft result = ignore
                                          | bufferLength < 19 = getMore
                                          | bufferLength < len = getMore
                                          | marker /= lBGPMarker = finish "Bad marker"
                                          | otherwise = return $ BufferedSocket sock newBuffer (Right $ BGPByteString newMsg)
                                          where
    bufferLength = L.length buffer
    marker = L.take 16 buffer
    len = fromIntegral $ getWord16 (L.take 2 $ L.drop 16 buffer)
    (rawMsg,newBuffer) = L.splitAt len buffer
    newMsg = L.drop 18 rawMsg 
    ignore = do putStrLn "getNext called on finished stream"
                return (BufferedSocket sock buffer result)
    finish s = return $ BufferedSocket sock buffer (Left s)
    getMore = do
        more <- L.recv sock 4096
        if L.null more then
            return $ BufferedSocket sock buffer (Left "end of stream")
        else
            getNext $ BufferedSocket sock (buffer `L.append` more) result
    getWord16 :: L.ByteString -> Word16
    getWord16 lbs = getWord16' $ map fromIntegral (L.unpack lbs)
    getWord16' :: [Word16] -> Word16
    getWord16' (l0:l1:_) = l1 .|. unsafeShiftL l0 8

!lBGPMarker = L.replicate 16 0xff
!_BGPMarker = B.replicate 16 0xff
instance Binary BGPByteString where 

    put (BGPByteString bs) = do
        putLazyByteString lBGPMarker
        putWord16be (fromIntegral $ L.length bs +18)
        putLazyByteString bs

    get = label "BGPByteString" $ do
        empty <- isEmpty
        when empty (fail "BGP end of stream")
        marker <- getLazyByteString 16
        unless ( marker == lBGPMarker ) (fail "BGP marker synchronisation error")
        len <- getWord16be
        bs <- getLazyByteString (fromIntegral len)
        return (BGPByteString bs)


instance {-# OVERLAPPING #-} Binary [BGPByteString] where
    put = putn
    get = getn

sndBgpMessage :: BufferedSocket -> L.ByteString -> IO ()
sndBgpMessage bsock bgpMsg = L.sendAll (socket bsock) $ encode (BGPByteString bgpMsg)
sndBgpMessage' :: BufferedSocket -> BGPByteString -> IO ()
sndBgpMessage' bsock bgpMsg = L.sendAll (socket bsock) (encode bgpMsg)

getBgpMessage' :: BufferedSocket -> IO BGPByteString
getBgpMessage' bsock = do
    bgpMsg <- getBgpMessage bsock
    return $ BGPByteString bgpMsg

getBgpMessage :: BufferedSocket -> IO L.ByteString
getBgpMessage bsock = do -- putStrLn "getBgpMessage"
    msgHdr <- recv' (socket bsock) 18
    let (marker,lenW) = L.splitAt 16 msgHdr
        l0 = fromIntegral $ L.index lenW 0 :: Word16
        l1 = fromIntegral $ L.index lenW 1 :: Word16
        len = l1 .|. unsafeShiftL l0 8
        bodyLength = fromIntegral len - 18
    -- putStrLn $ "payload length: " ++ show bodyLength
    unless (marker == lBGPMarker)
           (fail "Bad marker")
    body <- recv' (socket bsock) bodyLength
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

