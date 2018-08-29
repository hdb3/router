{-# LANGUAGE RecordWildCards #-}
module Update(processUpdate,getUpdate,ParsedUpdate(..)) where
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
import BGPData

-- 'hash' will become 'routeId' when it is inserted into the RouteData record....
data ParsedUpdate = ParsedUpdate { puPathAttributes :: [PathAttribute], nlri :: [Prefix], withdrawn :: [Prefix], hash :: Int } deriving Show

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

getRoute :: ParsedUpdate -> RouteData
getRoute ParsedUpdate{..} = makeRouteData undefined puPathAttributes hash

makeRouteData peerData pathAttributes routeId = RouteData peerData pathAttributes routeId pathLength nextHop origin med fromEBGP
    where
    pathLength = getASPathLength pathAttributes
    fromEBGP = isExternal peerData
    med = if fromEBGP then 0 else getMED pathAttributes
    nextHop = getNextHop pathAttributes
    origin = getOrigin pathAttributes

getUpdate :: BGPMessage -> ParsedUpdate
getUpdate BGPUpdate{..} = ParsedUpdate { puPathAttributes = a , nlri = n , withdrawn = w,
                                        hash = fromIntegral $ hash64 (L.toStrict attributes)  }
                               where (a,n,w) = validResult $ parseUpdate attributes nlri withdrawn

processUpdate a n w = 
    let parsedResult = parseUpdate a n w
        parsedUpdate = validResult parsedResult
    in
    if parseSuccess parsedResult then (Just parsedUpdate)
    else Nothing
{- informative error message is:
        "parsing failed: "
        parseErrorMesgs parsedResult
        diagoseResult parsedResult (a,n,w)
-}
