{-# LANGUAGE RecordWildCards #-}
-- module Update where
module Update(processUpdate,getUpdateP,BGPUpdateP(..)) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Binary
import Data.Either
import Control.Monad(when)
import FarmHash(hash64) -- from package farmhash

import Common
import RFC4271
import Open
import Capabilities
import PathAttributes
import Prefixes
import BGPparse

data BGPUpdateP = BGPUpdateP { attributesP :: [PathAttribute], nlriP :: [Prefix], withdrawnP :: [Prefix], hashP :: Int } deriving Show

{-
newtype RawAttributes = RawAttributes B.ByteString deriving Eq
instance Show RawAttributes where
    show (RawAttributes raw) = "RawAttributes [" ++ show (B.length raw ) ++ "]"
-}
type Update = ([PathAttribute],[Prefix],[Prefix])
parseUpdate a n w = (decodedAttributes,decodedNlri,decodedWithdrawn)
    where
        decodedAttributes = decodeOrFail a :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [PathAttribute])
        decodedNlri = decodeOrFail n :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [Prefix])
        decodedWithdrawn = decodeOrFail w :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, [Prefix])

parseSuccess (a,n,w) = isRight a && isRight n && isRight w
parseErrorMesgs (a,n,w) = concat [getMsgA a,getMsgP n,getMsgP w]
    where getMsgP (Right _) = ""
          getMsgP (Left(_,_,s)) = s
          getMsgA (Right _) = ""
          getMsgA (Left(_,_,s)) = s
validResult (a,n,w) = (f a,f n, f w) where f = (\(Right(_,_,x)) ->x)
validAttributes (a,n,w) = (null n && null a) || checkForRequiredPathAttributes a
endOfRIB (a,n,w) = null a && null n && null w

diagoseResult (a',n',w') (a,n,w) = diagnose "attributes" a' a ++
                                   diagnose "NLRI" n' n ++
                                   diagnose "withdrawn" w' w where
    diagnose _ (Right _) _ = ""
    diagnose t (Left (_,n,s)) x = "Error parsing " ++ t ++ " at position " ++ show n ++ "\n" ++ toHex' x

verbose (a,n,w) = do
    putStrLn "attributes"
    print a
    putStrLn "nrli"
    print n
    putStrLn "withdrawn"
    print w
    putStrLn "---------------------"

getUpdateP :: BGPMessage -> BGPUpdateP
getUpdateP BGPUpdate{..} = BGPUpdateP { attributesP = a , nlriP = n , withdrawnP = w,
                                        hashP = fromIntegral $ hash64 (L.toStrict attributes)  }
                               where (a,n,w) = validResult $ parseUpdate attributes nlri withdrawn
-- getRaw :: BGPUpdateP -> B.ByteString
-- getRaw BGPUpdateP{..} = fromRaw rawAttributes
-- fromRaw (RawAttributes raw) = raw
-- fromRaw' = L.fromStrict . fromRaw

processUpdate a n w v = do
-- 'v' is the verbose flag
    let parsedResult = parseUpdate a n w
        parsedUpdate = validResult parsedResult
    if parseSuccess parsedResult then do
        when v (putStrLn "Parse success")
        if not (validAttributes parsedUpdate) then do
            putStrLn "***Invalid Update!!!"
            verbose parsedUpdate
        else if endOfRIB parsedUpdate then
            putStrLn "End-of-RIB"
        else if v then
            verbose parsedUpdate
        else
            -- putChar '.'
            return ()
        return (Just parsedUpdate)
    else do
        putStr "parsing failed: "
        putStrLn $ parseErrorMesgs parsedResult
        putStrLn $ diagoseResult parsedResult (a,n,w)
        return Nothing
