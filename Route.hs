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
import Control.Monad.Extra(concatMapM)
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

lookupRoutes :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPMessage]
lookupRoutes rib peer ares = do routes <- concatMapM (lookupRoute rib peer) ares
                                -- mapM print routes
                                return $ map ungetUpdate routes

lookupRoute :: Rib -> PeerData -> AdjRIBEntry -> IO [ ParsedUpdate ]
lookupRoute rib peer (iprefixes, 0 ) = return [ originateWithdraw $ toPrefixes iprefixes ]
lookupRoute rib peer ([], routeId ) = do
    putStrLn ("empty prefix list in lookupRoute")
    return []

lookupRoute rib peer (iprefixes, routeId ) = do
    maybeRoute <- queryRib rib (head iprefixes)
    maybe (do putStrLn "failed lookup in lookupRoute"
              return []
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
              in return $ if isExternal peer then egpUpdate else igpUpdate
          )
          maybeRoute
