{-# LANGUAGE FlexibleInstances,BangPatterns,RecordWildCards #-}
module GetBGPMsg where

import System.Timeout(timeout)
import System.IO.Error(catchIOError)
import System.IO(Handle,hClose,hFlush,IOMode(ReadWriteMode))
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Either(isLeft)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket(socketToHandle, Socket)
import qualified Network.Socket.ByteString.Lazy as L
import Control.Monad(when,unless,fail)
import Data.ByteString.Builder
import Data.Monoid((<>))

import Common

data RcvStatus =   Timeout | EndOfStream | Error String deriving (Eq,Show)

newtype BGPByteString = BGPByteString (Either RcvStatus L.ByteString) deriving Eq

rcvStatus (BGPByteString (Left status)) = show status
rcvStatus (BGPByteString (Right bs)) = toHex' bs

data BufferedSocket = BufferedSocket {rawSocket :: Socket, handle :: Handle, result :: BGPByteString, inputFile :: Maybe Handle }
newBufferedSocket ::  Socket -> Maybe Handle -> IO BufferedSocket
newBufferedSocket sock h = do handle <- socketToHandle sock ReadWriteMode
                              return $ BufferedSocket sock handle (BGPByteString $ Right L.empty) h

getMsg :: BufferedSocket -> Int -> IO (BufferedSocket,BGPByteString)
getMsg b t = do next <- getNextTimeout t b
                return (next,result next)

getNextTimeout :: Int -> BufferedSocket -> IO BufferedSocket
getNextTimeout' t = getNext
getNextTimeout t bsock = let t' = t * 1000000 in
             do resMaybe <- timeout t' (getNext bsock)
                maybe
                    (return (bsock {result = BGPByteString $ Left Timeout} ))
                    return
                    resMaybe

getNext:: BufferedSocket -> IO BufferedSocket
getNext b = catchIOError (getNext' b)
                         (\e -> return (b {result = BGPByteString $ Left (Error (show e))} ))
             
getNext':: BufferedSocket -> IO BufferedSocket
getNext' bs@(BufferedSocket sock sHandle (BGPByteString result) handle) = do
    nextMsg <- getNextMsg sHandle
    return  $ BufferedSocket sock sHandle (BGPByteString $ Right nextMsg) handle
    where

    getWord16 :: L.ByteString -> Word16
    getWord16 lbs = getWord16' $ map fromIntegral (L.unpack lbs)
    getWord16' :: [Word16] -> Word16
    getWord16' (l0:l1:_) = l1 .|. unsafeShiftL l0 8

    getNextMsg h = do
        header <- L.hGet sHandle 18
        if  L.length header < 18 then 
            return L.empty
        else do
            let (m,l) = L.splitAt 16 header
            body <-  L.hGet sHandle (fromIntegral $ getWord16 l - 18)
            return body

!lBGPMarker = L.replicate 16 0xff
!_BGPMarker = B.replicate 16 0xff

instance Binary BGPByteString where 

    put (BGPByteString (Right bs)) | msgLength > 4096 = fail $ "trying to put an overlong BGPByteString, " ++ show msgLength ++ " bytes"
                                   | otherwise = do
        putLazyByteString lBGPMarker
        putWord16be msgLength
        putLazyByteString bs where
            msgLength = fromIntegral $ L.length bs + 18

    put (BGPByteString (Left _)) = fail "trying to but an invalid BGPByteString"

    get = label "BGPByteString" $ do
        empty <- isEmpty
        when empty (fail "BGP end of stream")
        marker <- getLazyByteString 16
        unless ( marker == lBGPMarker ) (fail "BGP marker synchronisation error")
        len <- getWord16be
        unless ( len < 4097 ) (fail "BGP message length invalid")
        bs <- getLazyByteString (fromIntegral (len-18))
        return (BGPByteString $ Right bs)

getBGPByteString :: Get BGPByteString
getBGPByteString = get

getBGPByteStrings :: Get [BGPByteString]
getBGPByteStrings = get

instance {-# OVERLAPPING #-} Binary [BGPByteString] where
    put = putn
    get = getn

-- simple replacement for Binary instance of BGPByteString
wireFormat :: L.ByteString -> L.ByteString
wireFormat bs = toLazyByteString $ lazyByteString lBGPMarker <> word16BE (fromIntegral $ 18 + L.length bs) <> lazyByteString bs 

sndBgpMessage :: BufferedSocket -> L.ByteString -> IO ()
sndBgpMessage bsock bgpMsg = L.hPut (handle bsock) $ wireFormat bgpMsg
