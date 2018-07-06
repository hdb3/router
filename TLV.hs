module TLV where
import Data.Word
import Data.ByteString.Builder
import Data.Monoid((<>))
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A

-- 
-- parsing and deparsing between (type/value) pairs as bytestrings of concatenated TLVs and [(Word8,ByteString)]
--
-- the outer deparser is specialised to the case of the BGP Open message which expects all usefull stuff to be encoded in a singer outer TLV with a fixed Type Code
--

deparser :: [(Word8,ByteString)] -> Builder
deparser = foldMap (\(t,v) -> word8 t <> word8 (fromIntegral $ B.length v) <> byteString v)

innerDeparser :: [(Word8,ByteString)] -> L.ByteString
innerDeparser tlvs = toLazyByteString $ deparser tlvs

outerDeparser :: [(Word8,ByteString)] -> ByteString

outerDeparser tlvs = L.toStrict $ toLazyByteString $ word8 2 <> word8 len <> lazyByteString innerBS where
                     innerBS = innerDeparser tlvs
                     len = fromIntegral $ L.length innerBS

openParser :: ByteString -> Either String [(Word8,ByteString)]
openParser bs =
    either Left
           (A.parseOnly tlvsParser)
           (A.parseOnly outerParser bs)
    
outerParser :: A.Parser ByteString
outerParser =  do
    t <- A.word8 2
    l <- A.anyWord8
    A.take (fromIntegral l)

innerParser = A.parseOnly tlvsParser
tlvsParser :: A.Parser [(Word8,ByteString)]
tlvsParser = A.many' tlvParser
tlvParser :: A.Parser (Word8,ByteString)
tlvParser =  do
    t <- A.anyWord8
    l <- A.anyWord8
    v <- A.take (fromIntegral l)
    return (t,v)
