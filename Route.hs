module Route where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode,decode,decodeOrFail)
import Control.Concurrent
import Control.Exception
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust)
import Data.Either(either)
import Data.Int(Int64)

import Common
import BGPparse
import BGPData
import GetBGPMsg
import RFC4271
import Open
import Capabilities
import Collision
import Update
import PathAttributes
import Prefixes
import Rib
import PrefixTableUtils
import AdjRIBOut

lookupRoutes :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPMessage]
-- lookupRoutes _ _ _ = return [BGPKeepalive]
lookupRoutes rib peer updates = mapM (lookupRoute rib peer) updates

lookupRoute :: Rib -> PeerData -> AdjRIBEntry -> IO BGPMessage
-- lookupRoute _ _ _ = return BGPKeepalive
lookupRoute rib peer (iprefixes, 0 ) = return $ ungetUpdate $ originateWithdraw $ toPrefixes iprefixes
lookupRoute rib peer (iprefixes, routeId ) = do
    maybeRoute <- queryRib rib (head iprefixes)
    let route = fromJust $ maybeRoute
        update = updateRoute (pathAttributes route) Nothing Nothing Nothing (toPrefixes iprefixes)
        update' = updateRoute (pathAttributes route)
                              (Just _BGP_ORIGIN_INCOMPLETE)
                              (Just $ myAS $ globalData $ peerData route )
                              ( Just $ nextHop route )
                              (toPrefixes iprefixes)
    return $ ungetUpdate update
-- originateUpdate :: Word8 -> [ASSegment Word32] -> IPv4 -> [Prefix] -> ParsedUpdate
-- updateRoute :: [PathAttribute] -> Maybe Word8 -> Maybe Word32 -> Maybe IPv4 -> [Prefix] -> ParsedUpdate

