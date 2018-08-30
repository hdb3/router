{-# LANGUAGE RecordWildCards #-}
module Update(updateRoute, processUpdate,getUpdate,ungetUpdate,ParsedUpdate(..),igpUpdate,originateWithdraw,originateUpdate) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Binary
import Data.Either
import Control.Monad(when)

import Common
import RFC4271
import Open
import Capabilities
import PathAttributes
import Prefixes
import BGPparse

-- 'hash' will become 'routeId' when it is inserted into the RouteData record....
data ParsedUpdate = ParsedUpdate { puPathAttributes :: [PathAttribute], nlri :: [Prefix], withdrawn :: [Prefix], hash :: Int } deriving Show

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

-- BGPUpdate { withdrawn :: L.ByteString, attributes :: L.ByteString, nlri :: L.ByteString }

ungetUpdate :: ParsedUpdate -> BGPMessage
ungetUpdate ParsedUpdate{..} = BGPUpdate { withdrawn = encode withdrawn , attributes = encode puPathAttributes , nlri = encode nlri } 
-- ungetUpdate ( ParsedUpdate puPathAttributes puNlri puWithdrawn _ ) = BGPUpdate { withdrawn = encode puWithdrawn , attributes = encode puPathAttributes , nlri = encode puNlri } 

getUpdate :: BGPMessage -> ParsedUpdate
getUpdate BGPUpdate{..} = ParsedUpdate { puPathAttributes = a , nlri = n , withdrawn = w,
                                        hash = myHash attributes  }
                               where (a,n,w) = validResult $ parseUpdate attributes nlri withdrawn

-- TODO clean up the mess here around error handling.....
processUpdate :: BGPMessage -> Maybe ParsedUpdate
processUpdate ( BGPUpdate w a n ) = 
    let parsedResult = parseUpdate a n w
        (puPathAttributes,nlri,withdrawn) = validResult parsedResult
        hash = myHash a
    in
    if parseSuccess parsedResult then Just (ParsedUpdate puPathAttributes nlri withdrawn hash)
    else Nothing
{- informative error message is:
        "parsing failed: "
        parseErrorMesgs parsedResult
        diagoseResult parsedResult (a,n,w)
-}

originateWithdraw prefixes = ParsedUpdate []  [] prefixes 0

updateRoute :: [PathAttribute] -> Maybe Word8 -> Maybe Word32 -> Maybe IPv4 -> [Prefix] -> ParsedUpdate
updateRoute attributes origin maybeAS maybeNextHop prefixes = ParsedUpdate attributes' prefixes [] hash where
    -- attributes' = attributes
    attributes' = updateOrigin origin $ updateNextHop maybeNextHop $ updatePath maybeAS attributes :: [PathAttribute]
    updateOrigin Nothing b = b
    updateNextHop Nothing b = b
    updatePath Nothing b = b
    -- updateOrigin a b = b
    -- updateNextHop a b = b
    -- updatePath a b = b
    hash = myHash $ encode attributes'

originateUpdate :: Word8 -> [ASSegment Word32] -> IPv4 -> [Prefix] -> ParsedUpdate
originateUpdate origin path nextHop prefixes = ParsedUpdate attributes prefixes [] hash where
    attributes = [PathAttributeOrigin origin, PathAttributeASPath (ASPath4 path), PathAttributeNextHop nextHop]
    hash = myHash $ encode attributes

igpUpdate = originateUpdate _BGP_ORIGIN_IGP []
