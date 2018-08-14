module BGPData where

{- peer data holds persistent/static data about a BGP session peer
 - this can be useful for BGP operatins like path selection
-}

import Data.Word
import Data.IP(IPv4)

data GlobalData = GlobalData { myAS :: Word32 , myBGPid :: IPv4 } deriving (Show,Eq)

data PeerData = PeerData { globalData :: GlobalData
                         ,  isExternal :: Bool
                         ,  peerAS :: Word32
                         ,  peerBGPid :: IPv4
                         ,  peerIPv4 :: IPv4
                         ,  localIPv4 :: IPv4
                         ,  localPref :: Word32
                         } deriving (Show,Eq)

data RouteData =  RouteData { peerData :: PeerData
                            , pathLength :: Word8
                            , origin :: Word8
                            , med :: Word32
                            , fromEBGP :: Bool
                            } deriving (Show,Eq)



instance Ord RouteData where

  compare rd1 rd2 = compare (localPref (peerData rd1), pathLength rd1, origin rd1, med rd1, not $ fromEBGP rd1, peerBGPid (peerData rd1), peerIPv4 (peerData rd1))
                            (localPref (peerData rd2), pathLength rd2, origin rd2, med rd2, not $ fromEBGP rd2, peerBGPid (peerData rd2), peerIPv4 (peerData rd2))
{-
data Cost = Cost { preference :: Word32, pathLength :: Word8, med :: Word32, fromEBGP :: Bool, peerBGPID :: Word32 } deriving Eq
      | pr1 /= pr2 = compare pr1 pr2
      | pl1 /= pl2 = compare pl1 pl2
      | med1 /= med2 = compare med1 med2
      | fEBGP1 /= fEBGP2 = compare fEBGP1 fEBGP2
      | otherwise = compare peer1 peer2
-}
