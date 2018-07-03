module TLV where
import Data.Word
import Data.ByteString.Builder
import Data.Monoid((<>))
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
-- import Data.ByteString.Lazy()
import qualified Data.ByteString.Lazy as L

import qualified Data.Attoparsec.ByteString as A

-- parse/deparse the Open message, especially the optional parametes//capabilities
-- the optional parameter field has a (8bit) length sub-field followed by 0 or more 'parameters
-- parameters are coded as TLVs with 8bit T and L
-- only one parameter is currently used - '2' == Capabilities Optional Parameter
-- (see RFC5492)
-- within the 'Capabilities Optional Parameter' is another TLV encoding,
-- although there is no length field to start
--
-- a praser yields a list of (t,v) capability pairs:w
--
-- the deparser takes a list of tuples and produces a bytestring
-- in this instrance the outerTLV structure is hardcoded...
--

-- deparser :: [(Word8,ByteString)] -> ByteString
deparser :: [(Word8,ByteString)] -> Builder
deparser tlvs = foldMap (\(t,v) -> word8 t <> word8 (fromIntegral $ B.length v) <> byteString v) tlvs

innerDeparser :: [(Word8,ByteString)] -> L.ByteString
innerDeparser tlvs = toLazyByteString $ deparser tlvs

outerDeparser :: [(Word8,ByteString)] -> ByteString

outerDeparser tlvs = L.toStrict $ toLazyByteString $ word8 2 <> word8 len <> lazyByteString innerBS where
                     innerBS = innerDeparser tlvs
                     len = fromIntegral $ L.length innerBS

-- openDeparser :: [(Word8,ByteString)] -> ByteString
-- openDeparser tlvs = L.toStrict $ toLazyByteString $ word8 len <> lazyByteString outerBS where
                    -- outerBS = outerDeparser tlvs
                    -- len = fromIntegral $ L.length outerBS

openParser :: ByteString -> Either String [(Word8,ByteString)]
openParser bs =
    either Left
           (A.parseOnly tlvsParser)
           (A.parseOnly outerParser bs)
    
outerParser :: A.Parser ByteString
outerParser =  do
    t <- A.word8 2
    l <- A.anyWord8
    v <- A.take (fromIntegral l)
    return v
innerParser = A.parseOnly tlvsParser
tlvsParser :: A.Parser [(Word8,ByteString)]
tlvsParser = A.many' tlvParser
tlvParser :: A.Parser (Word8,ByteString)
tlvParser =  do
    t <- A.anyWord8
    l <- A.anyWord8
    v <- A.take (fromIntegral l)
    return (t,v)
