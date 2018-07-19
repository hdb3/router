{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances,BangPatterns #-}
-- module GetBGPMsg where
module GetBGPMsg (RcvStatus(..),BufferedSocket(..),newBufferedSocket,rcvStatus,getMsg,getNext,sndBgpMessage,BGPByteString(..)) where

{- BGP messages once received are bytestrings - 
 - removing the static marker and length fields is
 - an obvious simplification.   Subsequent parsers
 - need not manage them.  However this means that binary represntations will need to have
 - the marker and length field reapplied before transmission on the network.
-}

import System.Timeout(timeout)
import System.IO.Error(catchIOError)
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

data RcvStatus =   Timeout | EndOfStream | Error String deriving (Eq,Show)

data BGPByteString = BGPByteString (Either RcvStatus L.ByteString)

rcvStatus (BGPByteString (Left status)) = status
-- rcvStatus (BGPByteString bgpE) = fromLeft bgpE

data BufferedSocket = BufferedSocket {rawSocket :: Socket, buf :: L.ByteString, result :: BGPByteString}
newBufferedSocket sock = BufferedSocket sock L.empty (BGPByteString $ Right L.empty)

-- convenince functions...
-- get' :: BufferedSocket -> Int -> IO (BufferedSocket,BGPMessage)
-- get' b t = do (next,bytes) <- get b t
--               return (next, decode bytes :: BGPMessage)
getMsg :: BufferedSocket -> Int -> IO (BufferedSocket,BGPByteString)
getMsg b t = do next <- getNextTimeout t b
                return (next,result next)

-- core functions...

getNextTimeout :: Int -> BufferedSocket -> IO BufferedSocket
getNextTimeout' t b = getNext b
getNextTimeout t bsock = let t' = t * 10000000 in
             do resMaybe <- timeout t' (getNext bsock)
                maybe
                    (return (bsock {result = BGPByteString $ Left Timeout} ))
                    return
                    resMaybe

getNext:: BufferedSocket -> IO BufferedSocket
getNext b = catchIOError (getNext' b)
                         (\e -> do -- can get rid of error to screen if the response is displayed elsewhere
                                   putStrLn $ "IOError in get: " ++ show (e :: IOError)
                                   return (b {result = BGPByteString $ Left (Error (show e))} ))
             
getNext':: BufferedSocket -> IO BufferedSocket
getNext' bs@(BufferedSocket sock buffer (BGPByteString result))
                                          -- possibly should not have this check at all...
                                          -- if the application wants to try again?
                                          | isLeft result && result /= (Left Timeout) = ignore
                                          | bufferLength < 19 = getMore
                                          | bufferLength < len = getMore
                                          | marker /= lBGPMarker = return $ bs {result = BGPByteString $ Left $ Error "Bad marker in GetBGPByteString"}
                                          | otherwise = return $ BufferedSocket sock newBuffer (BGPByteString $ Right newMsg)
                                          where
    bufferLength = L.length buffer
    marker = L.take 16 buffer
    len = fromIntegral $ getWord16 (L.take 2 $ L.drop 16 buffer)
    (rawMsg,newBuffer) = L.splitAt len buffer
    newMsg = L.drop 18 rawMsg 
    ignore = do putStrLn "getNext called on finished stream"
                return bs -- (BufferedSocket sock buffer result)
    getMore = do
        more <- L.recv sock 4096
        if L.null more then
            return $  bs {result= BGPByteString $ Left EndOfStream } -- BufferedSocket sock buffer (Left EndOfStream)
        else
            getNext' $ bs {buf = buffer `L.append` more} -- BufferedSocket sock (buffer `L.append` more) result
    getWord16 :: L.ByteString -> Word16
    getWord16 lbs = getWord16' $ map fromIntegral (L.unpack lbs)
    getWord16' :: [Word16] -> Word16
    getWord16' (l0:l1:_) = l1 .|. unsafeShiftL l0 8

!lBGPMarker = L.replicate 16 0xff
!_BGPMarker = B.replicate 16 0xff
instance Binary BGPByteString where 

    put (BGPByteString (Right bs)) = do
        putLazyByteString lBGPMarker
        putWord16be (fromIntegral $ L.length bs +18)
        putLazyByteString bs
    put (BGPByteString (Left _)) = fail "trying to but an invalid BGPByteString"

    get = label "BGPByteString" $ do
        empty <- isEmpty
        when empty (fail "BGP end of stream")
        marker <- getLazyByteString 16
        unless ( marker == lBGPMarker ) (fail "BGP marker synchronisation error")
        len <- getWord16be
        bs <- getLazyByteString (fromIntegral len)
        return (BGPByteString $ Right bs)


instance {-# OVERLAPPING #-} Binary [BGPByteString] where
    put = putn
    get = getn

sndBgpMessage :: BufferedSocket -> L.ByteString -> IO ()
sndBgpMessage bsock bgpMsg = L.sendAll (rawSocket bsock) $ encode (BGPByteString $ Right bgpMsg)
-- sndBgpMessage' :: BufferedSocket -> BGPByteString -> IO ()
-- sndBgpMessage' bsock bgpMsg = L.sendAll (rawSocket bsock) (encode bgpMsg)
