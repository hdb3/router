module GetTLVs where
import qualified Data.ByteString as B
import Data.Word

-- import Data.ByteString.Unsafe(unsafeIndex)

getTLVs :: B.ByteString -> [B.ByteString]
getTLVs bs | B.null bs = []
           | B.length bs > 1 = tlv : getTLVs bs' where
                 -- l = fromIntegral $ 2 + unsafeIndex bs 1
                 l = fromIntegral $ 2 + B.index bs 1
                 (tlv,bs') = B.splitAt l bs

parseTLV :: B.ByteString -> (Word8,B.ByteString)
parseTLV bs | lbs > 1 && lbs == l+2 = (B.index bs 0, B.drop 2 bs) where
    lbs = B.length bs
    l = fromIntegral $ 2 + B.index bs 1
