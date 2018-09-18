module Global where

import Control.Concurrent
import Network.Socket
import qualified Data.Map.Strict as Data.Map

import Common
import BGPData
import Collision
import Rib

data Global = Global { rib :: Rib.Rib
                     , peerMap :: Data.Map.Map IPv4 PeerData
                     ,collisionDetector :: CollisionDetector
                     ,sessions :: MVar ( Data.Map.Map ThreadId PeerData )
                     ,gd :: GlobalData
                     ,ld :: PeerData
                     ,listenAddress :: SockAddr
                     , peers :: [PeerData]
                     , delayOpenTimer :: Int
                     , initialHoldTimer :: Int
                     , defaultPeerData :: Maybe PeerData -- used to configure dynamic peers
                     }



type FSMExit = ( ThreadId, SockAddr, Either String String )
