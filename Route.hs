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
import PathAttributeUtils
import Prefixes
import Rib
import PrefixTableUtils
import AdjRIBOut

lookupRoutes :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPMessage]
lookupRoutes rib peer = mapM (lookupRoute rib peer)

lookupRoute :: Rib -> PeerData -> AdjRIBEntry -> IO BGPMessage
lookupRoute rib peer (iprefixes, 0 ) = return $ ungetUpdate $ originateWithdraw $ toPrefixes iprefixes
lookupRoute rib peer (iprefixes, routeId ) = do
    maybeRoute <- queryRib rib (head iprefixes)
    let route = fromJust maybeRoute
        igpUpdate = makeUpdate (toPrefixes iprefixes)
                               []
                               ( setOrigin _BGP_ORIGIN_INCOMPLETE $
                                   setNextHop (nextHop route) $
                                   setLocalPref (localPref $ peerData route) $
                                   pathAttributes route
                                 )
        egpUpdate = makeUpdate (toPrefixes iprefixes)
                               []
                               ( setOrigin _BGP_ORIGIN_INCOMPLETE $
                                   setNextHop (nextHop route) $
                                   prePendAS ( myAS $ globalData $ peerData route) $
                                   pathAttributes route
                                 )
    return $ ungetUpdate $ if isExternal peer then egpUpdate else igpUpdate
