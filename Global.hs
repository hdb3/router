module Global where

import Control.Concurrent
import Network.Socket
import qualified Data.IP
import qualified Data.Map.Strict as Data.Map

import Common
import BGPData
import Collision
import Args2
import Rib
import BGPReader(pathReadRib)
import Update(makeUpdate)

data Global = Global { rib :: Rib.Rib
                     , peerMap :: Data.Map.Map IPv4 PeerData
                     ,exitMVar :: MVar (ThreadId, SockAddr, Either String String)
                     ,collisionDetector :: CollisionDetector
                     ,sessions :: MVar ( Data.Map.Map ThreadId PeerData )
                     ,gd :: GlobalData
                     ,ld :: PeerData
                     ,listenAddress :: SockAddr
                     , peers :: [PeerData]
                     -- , config :: BgpFSMconfig
                     , delayOpenTimer :: Int
                     }



type FSMExit = ( ThreadId, SockAddr, Either String String )

data BgpFSMconfig = BgpFSMconfig {
                                  sock :: Socket
                                  , peerName :: SockAddr
                                  -- , logFile :: Maybe Handle
                                  -- , peerData :: PeerData
                                  }

