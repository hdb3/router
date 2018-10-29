{-# LANGUAGE FlexibleInstances #-}
module RIBData where

import Data.Word
import Data.IP(IPv4)

import BGPlib

data Peer = Peer {  peerName :: String
                 ,  isExternal :: Bool
                 ,  peerAS :: Word32
                 ,  peerBGPid :: IPv4
                 ,  peerIPv4 :: IPv4
                 ,  localIPv4 :: IPv4
                 }

data Route =  Route { routeName :: String
                    , localPref :: Word32
                    , pathAttributes :: [PathAttribute]
                    , pathLength :: Int
                    , origin :: Word8
                    , med :: Word32
                    , fromEBGP :: Bool
                    }
instance Show Peer where
    show p = "Peer \"" ++ peerName p ++ "\""

instance Show Route where
    show r = "Route \"" ++ routeName r ++ "\""

instance Eq Route where
    _ == _ = True -- dummy instance because we must have one for the later ord function....

instance Eq Peer where
    p1 == p2 = peerBGPid p1 == peerBGPid p2

instance Ord Peer where
    compare p1 p2 = compare ( peerBGPid p1 ) ( peerBGPid p2 )

instance {-# OVERLAPPING #-} Ord (Peer,Route) where

  compare (pd1,rd1) (pd2,rd2) = compare (localPref rd1, pathLength rd1, origin rd1, med rd1, not $ fromEBGP rd1, peerBGPid pd1, peerIPv4 pd1)
                                        (localPref rd2, pathLength rd2, origin rd2, med rd2, not $ fromEBGP rd2, peerBGPid pd2, peerIPv4 pd2)
