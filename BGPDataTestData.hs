{-#LANGUAGE OverloadedStrings #-}
module BGPDataTestData where

import Data.Word
import Data.IP(IPv4)

import RFC4271
import BGPData

gd1 = GlobalData 65501 "172.16.0.1"
gd2 = GlobalData 65502 "172.16.0.2"

gd1Peer1 = PeerData { globalData = gd1
                    , isExternal = True
                    , peerAS  = 65502
                    , peerBGPid = "172.16.0.2"
                    ,  peerIPv4 = "192.168.0.2"
                    ,  localIPv4 = "192.168.0.1"
                    ,  localPref = 100
                    }

gd1Peer2 = gd1Peer1 { peerAS  = 65503
                    , peerBGPid = "172.16.0.3"
                    , peerIPv4 = "192.168.0.3"
                    }

gd2Peer1 = PeerData { globalData = gd2
                    , isExternal = True
                    , peerAS  = 65501
                    , peerBGPid = "172.16.0.1"
                    ,  peerIPv4 = "192.168.0.1"
                    ,  localIPv4 = "192.168.0.2"
                    ,  localPref = 100
                    }

gd2Peer2 = gd2Peer1 { peerAS  = 65504
                    , peerBGPid = "172.16.0.4"
                    , peerIPv4 = "192.168.0.4"
                    }


route11 = RouteData { peerData = gd1Peer1
                   , pathLength = 10
                   , origin = _BGP_ORIGIN_IGP
                   , med = 0
                   , fromEBGP = True
                   }
gd1Peer1Route1 = route11
gd1Peer2Route1 = gd1Peer1Route1 { peerData = gd2Peer2 }
gd1Peer1Route2 = gd1Peer1Route1 { pathLength = 11 }
gd1Peer2Route2 = gd1Peer1Route2 { peerData = gd2Peer2 }

route12 = route11 { peerData = gd1Peer2 }
route21 = route11 { peerData = gd2Peer1 }
route22 = route11 { peerData = gd2Peer2 }

route peer pl = route11 { peerData = peer, pathLength = pl }
