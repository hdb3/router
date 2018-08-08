module BGPData where

{- peer data holds persistent/static data about a BGP session peer
 - this can be useful for BGP operatins like path selection
-}

import Data.Word
data GlobalData = GlobalData { myAS :: Word32 }

data PeerData = PeerData { globalData :: GlobalData
                         ,  isExternal :: Bool
                         ,  bgpID :: Word32
                         ,  interfaceAddress :: Word32
                         ,  localPref :: Word32
                         }

data RouteData =  RouteData { peerData :: PeerData
                            , cost :: RouteCost
                            }
