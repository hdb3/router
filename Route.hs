module Route where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode,decode,decodeOrFail)
import Control.Concurrent
import Control.Exception
import Control.Monad(when,unless,liftM)
import Data.Maybe(fromJust,isJust,catMaybes)
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

lookupRoutes'' rib peer ares = do routes <- lookupRoutes' rib peer ares
                                  mapM (print . getUpdate) routes
                                  return routes

lookupRoutes = lookupRoutes'

lookupRoutes' :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPMessage]
lookupRoutes' rib peer ares = (liftM catMaybes) $ mapM (lookupRoute rib peer) ares

lookupRoute :: Rib -> PeerData -> AdjRIBEntry -> IO (Maybe BGPMessage)
lookupRoute rib peer (iprefixes, 0 ) = return $ Just $ ungetUpdate $ originateWithdraw $ toPrefixes iprefixes
lookupRoute rib peer ([], routeId ) = do
    putStrLn ("empty prefix list in lookupRoute")
    return Nothing

lookupRoute rib peer (iprefixes, routeId ) = do
    maybeRoute <- queryRib rib (head iprefixes)
    maybe (do putStrLn "failed lookup in lookupRoute"
              return Nothing
          )
          (\route -> let igpUpdate = makeUpdate (toPrefixes iprefixes)
                                                []
                                                ( sortPathAttributes $
                                                  setOrigin _BGP_ORIGIN_INCOMPLETE $
                                                  setNextHop (nextHop route) $
                                                  setLocalPref (localPref $ peerData route) $
                                                  pathAttributes route
                                                 )
                         egpUpdate = makeUpdate (toPrefixes iprefixes)
                                                []
                                                ( sortPathAttributes $
                                                  setOrigin _BGP_ORIGIN_INCOMPLETE $
                                                  setNextHop (nextHop route) $
                                                  prePendAS ( myAS $ globalData $ peerData route) $
                                                  pathAttributes route
                                                 )
              in return $ Just $ ungetUpdate $ if isExternal peer then egpUpdate else igpUpdate
          )
          maybeRoute
