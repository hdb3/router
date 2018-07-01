module GetBGPMsg (getBgpMessage) where

import Network.Socket hiding (recv, send)
import Network.Socket.ByteString(recv)
import Data.Binary.Get
-- import Data.Binary.Strict.Get
import Data.Word
import Data.Either
import Control.Monad(unless,fail)
import qualified Data.ByteString as B


_BGPMarker = B.replicate 16 0xff

parseBGPMessageHeader :: Get (Word8, Word16)
parseBGPMessageHeader = do
            marker <- getByteString 16
            unless (marker == _BGPMarker) (fail "Bad marker")
            msgLen <- getWord16be
            msgType <- getWord8
            return (msgType,msgLen)

getBgpMessage :: Socket -> IO (Either String (Word8, Word16, B.ByteString))
getBgpMessage sock = do msgHdr <- recv sock 19
                     print $ B.length msgHdr
                     let r = fst $ runGet parseBGPMessageHeader msgHdr
                     -- let (r,_) = runGet parseBGPMessageHeader msgHdr

                     either (\s -> return $ Left s)
                            ( \(msgType,msgLen) -> do msgBody <- recv sock $ fromIntegral msgLen
                                                      return $ Right (msgType,msgLen,msgBody))
                            -- r
                            ( fst $ runGet parseBGPMessageHeader msgHdr )
