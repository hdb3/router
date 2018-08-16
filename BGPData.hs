module BGPData where

{- peer data holds persistent/static data about a BGP session peer
 - this can be useful for BGP operatins like path selection
-}

import Data.Word
import Data.IP(IPv4)

data GlobalData = GlobalData { myAS :: Word32 
                             , myBGPid :: IPv4
                             }
                            deriving Eq

data PeerData = PeerData { globalData :: GlobalData
                         ,  isExternal :: Bool
                         ,  peerAS :: Word32
                         ,  peerBGPid :: IPv4
                         ,  peerIPv4 :: IPv4
                         ,  localIPv4 :: IPv4
                         ,  localPref :: Word32
                         }
                            deriving Eq

data RouteData =  RouteData { peerData :: PeerData
                            , pathLength :: Word8
                            , origin :: Word8
                            , med :: Word32
                            , fromEBGP :: Bool
                            }
                            deriving Eq

instance Show GlobalData where
    show gd = " router: " ++ show ( myBGPid gd )

instance Show PeerData where
    show pd = " peer-AS=" ++ show (peerAS pd) ++ " nexthop=" ++ show (peerIPv4 pd) --  ++ show (globalData pd)

instance Show RouteData where
    show rd = "pathlength=" ++ show (pathLength rd) ++ show (peerData rd)

instance Ord RouteData where

  compare rd1 rd2 = compare (localPref (peerData rd1), pathLength rd1, origin rd1, med rd1, not $ fromEBGP rd1, peerBGPid (peerData rd1), peerIPv4 (peerData rd1))
                            (localPref (peerData rd2), pathLength rd2, origin rd2, med rd2, not $ fromEBGP rd2, peerBGPid (peerData rd2), peerIPv4 (peerData rd2))
