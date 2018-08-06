{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module ASPath4 where
import Data.Binary
import Data.Word
import Data.List(foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import Data.Either.Combinators

import RFC4271
import Codes
import Common
import ASPath

decode = fromRight' . parseOnly (path <* endOfInput) . L.toStrict 
type ASSegment2 = ASSegment Word16
type ASSegment4 = ASSegment Word32

path :: Parser ASPath42
path = path4 <|> path2
path2 :: Parser ASPath42
path2 = do
    segs <- many' asSetOrSeq2
    endOfInput
    return $ ASPath2 $ ASPath segs

asSetOrSeq2 :: Parser ASSegment2
asSetOrSeq2 = do
    segType <- satisfy isSetOrSeq
    asCount <- anyWord8
    rvals <- count (fromIntegral asCount) anyWord16be
    return $ if segType == enumASSet then ASSet rvals else ASSequence rvals where
        isSetOrSeq b = b == enumASSet || b == enumASSequence

path4 :: Parser ASPath42
path4 = do
    segs <- many' asSetOrSeq4
    endOfInput
    return $ ASPath4 $ ASPath segs

asSetOrSeq4 :: Parser ASSegment4
asSetOrSeq4 = do
    segType <- satisfy isSetOrSeq
    asCount <- anyWord8
    rvals <- count (fromIntegral asCount) anyWord32be
    return $ if segType == enumASSet then ASSet rvals else ASSequence rvals where
        isSetOrSeq b = b == enumASSet || b == enumASSequence
