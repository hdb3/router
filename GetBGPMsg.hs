{-# LANGUAGE MagicHash, BangPatterns #-}
module GetBGPMsg (getBgpMessage,sndBgpMessage) where

{- BGP messages once received are bytestrings - 
 - removing the static marker and length fields is
 - an obvious simplification.   Subsequent parsers
 - need not manage them.  However this means that binary represntations will need to have
 - the marker and length field reapplied before transmission on the network.
-}

import Foreign
import GHC.Base
import GHC.Word
import Data.ByteString.Unsafe(unsafeIndex)
import Network.Socket hiding (recv, send)
import qualified Network.Socket.ByteString.Lazy as L
import qualified Network.Socket.ByteString as B
import Data.Binary.Get
import Data.Word
import Data.Either
import Control.Monad(unless,fail)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder
import           Data.Monoid
import Hexdump

_BGPMarker = B.replicate 16 0xff
sndBgpMessage :: Socket -> L.ByteString -> IO ()
sndBgpMessage sock bgpMsg = do let wireMessageLength = fromIntegral $ 18 + L.length bgpMsg
                                   wireMessage = L.toStrict $ toLazyByteString $ byteString _BGPMarker <>  word16BE wireMessageLength <> lazyByteString bgpMsg
                                   -- wireMessage = L.toStrict $ toLazyByteString $ byteString _BGPMarker <>  word16BE wireMessageLength <> byteString bgpMsg
                               B.sendAll sock wireMessage

getBgpMessage :: Socket -> IO L.ByteString
getBgpMessage sock = do msgHdr <- B.recv sock 18
                        let (marker,lenR) = B.splitAt 16 msgHdr
                            len = word16be lenR
                        unless (marker == _BGPMarker) (do putStr "Error::getBgpMessage:: "
                                                          print $ simpleHex msgHdr
                                                          putStrLn "----"
                                                          fail "Bad marker")
                        let bodyLength = fromIntegral len - 18
                        body <- B.recv sock bodyLength
                        unless (bodyLength == B.length body) (do let asked = show bodyLength
                                                                     got = show (B.length body)
                                                                 putStrLn $ "Error::getBgpMessage:: internal error - short read" ++
                                                                                      " - asked " ++ show (fromIntegral len) ++
                                                                                      " got " ++ show (B.length body)
                                                                 fail "internal error - short read")
                        return $ L.fromStrict body

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)

{-# INLINE word16be #-}
word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `unsafeIndex` 0) `shiftl_w16` 8) .|.
        (fromIntegral (s `unsafeIndex` 1))
{-# INLINE word32be #-}
word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `unsafeIndex` 3) )
