{-#LANGUAGE OverloadedStrings #-}
module BGPData where

{- peer data holds persistent/static data about a BGP session peer
 - this can be useful for BGP operatins like path selection
-}

import Data.Word
import Data.IP(IPv4)
import Data.Hashable

import PathAttributes(PathAttribute)
import Capabilities

data GlobalData = GlobalData { myAS :: Word32 
                             , myBGPid :: IPv4
                             -- TODO add a default local address - usually is going to be myBGPid but this might not be routable in some cases
                             }
                            deriving (Show,Eq)

data PeerData = PeerData { globalData :: GlobalData
                         ,  isExternal :: Bool
                         ,  peerAS :: Word32
                         ,  peerBGPid :: IPv4
                         ,  peerIPv4 :: IPv4
                         ,  localIPv4 :: IPv4
                         ,  localPref :: Word32
                         ,  propHoldTime :: Word16
                         ,  reqHoldTime :: Word16
                         ,  offerCapabilies :: [ Capability ]
                         ,  requireCapabilies :: [ Capability ]
                         }

data RouteData =  RouteData { peerData :: PeerData
                            , pathAttributes :: [PathAttribute]
                            , routeId :: Int
                            , pathLength :: Int
                            , nextHop :: IPv4
                            , origin :: Word8
                            , med :: Word32
                            , fromEBGP :: Bool
                            }

instance Hashable RouteData where
    hashWithSalt _ = routeId

localPeer :: GlobalData -> PeerData
localPeer gd = PeerData gd False (myAS gd) (myBGPid gd) (myBGPid gd) (myBGPid gd) 0 undefined undefined undefined undefined 
-- localPeer gd = PeerData gd false (myAS gd) (myBGPid gd) (myBGPid gd) (myBGPid gd) 0 0 0 [] [] 
nullRoute :: RouteData
nullRoute = RouteData undefined undefined 0 undefined undefined undefined undefined undefined
defaultPeerData :: PeerData
defaultPeerData = PeerData defaultGlobalData True 64513 "127.0.0.2" "127.0.0.2" "127.0.0.1" 0 100 0 [] []
defaultGlobalData :: GlobalData
defaultGlobalData = GlobalData 64512 "127.0.0.1"

--instance Show GlobalData where
--    show gd = " router: " ++ show ( myBGPid gd )

instance Show PeerData where
    show pd = " peer-AS=" ++ show (peerAS pd) ++ " peer-IP=" ++ show (peerBGPid pd)

instance Show RouteData where
    show rd = "pathlength=" ++ show (pathLength rd) ++ show (peerData rd)

instance Eq RouteData where
    a == b = routeId a == routeId b

instance Eq PeerData where
    p1 == p2 = peerBGPid p1 == peerBGPid p2

instance Ord PeerData where
    compare p1 p2 = compare (peerBGPid p1) (peerBGPid p2)

-- TODO - LocalPref should be a route not peer level value!!!!!
instance Ord RouteData where

  compare rd1 rd2 = compare (localPref (peerData rd1), pathLength rd1, origin rd1, med rd1, not $ fromEBGP rd1, peerBGPid (peerData rd1), peerIPv4 (peerData rd1))
                            (localPref (peerData rd2), pathLength rd2, origin rd2, med rd2, not $ fromEBGP rd2, peerBGPid (peerData rd2), peerIPv4 (peerData rd2))
