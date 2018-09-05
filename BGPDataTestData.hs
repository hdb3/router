{-#LANGUAGE OverloadedStrings #-}
module BGPDataTestData where

import Data.Word
import Data.IP(IPv4)

import RFC4271
import BGPData
import Capabilities

gd1 = GlobalData 65511 "172.16.0.254"
gd2 = GlobalData 65512 "172.16.0.253"

internalPeer = externalPeer {peerAS = (myAS (globalData externalPeer))}
externalPeer = gd1Peer1
gd1Peer1 = PeerData { globalData = gd1
                    , isExternal = True
                    , peerAS  = 65501
                    , peerBGPid = "172.16.0.1"
                    ,  peerIPv4 = "192.168.0.1"
                    ,  localIPv4 = "192.168.0.254"
                    ,  localPref = 100
                    ,  propHoldTime = 100
                    ,  reqHoldTime = 0
                    ,  offerCapabilies = [ CapAS4 (myAS gd1) ]
                    ,  requireCapabilies = [ CapAS4 65501 ]

                    }

gd1Peer2 = gd1Peer1 { peerAS  = 65502
                    , peerBGPid = "172.16.0.2"
                    , peerIPv4 = "192.168.0.2"
                    }

gd1Peer3 = gd1Peer1 { peerAS  = 65503
                    , peerBGPid = "172.16.0.3"
                    , peerIPv4 = "192.168.0.3"
                    }

gd2Peer1 = gd1Peer1 { globalData = gd2
                    , isExternal = True
                    , peerAS  = 65521
                    , peerBGPid = "172.16.0.1"
                    ,  peerIPv4 = "192.168.0.1"
                    ,  localIPv4 = "192.168.0.2"
                    ,  localPref = 100
                    }

gd2Peer2 = gd2Peer1 { peerAS  = 65522
                    , peerBGPid = "172.16.0.4"
                    , peerIPv4 = "192.168.0.4"
                    }


gd1Peer1Route1 = RouteData { peerData = gd1Peer1
                   , pathAttributes = []
                   , routeId = 99
                   , nextHop = "1.2.3.4"
                   , pathLength = 10
                   , origin = _BGP_ORIGIN_IGP
                   , med = 0
                   , fromEBGP = True
                   }
-- gd1Peer2Route1 = gd1Peer1Route1 { peerData = gd2Peer2 }
-- gd1Peer1Route2 = gd1Peer1Route1 { pathLength = 11 }
-- gd1Peer1Route3 = gd1Peer1Route1 { pathLength = 12 }
-- gd1Peer2Route2 = gd1Peer1Route2 { peerData = gd2Peer2 }

peer1 = gd1Peer1
peer2 = gd1Peer2
peer3 = gd1Peer3
route11 = gd1Peer1Route1
route12 = gd1Peer1Route1 { pathLength = 11 }
route13 = gd1Peer1Route1 { pathLength = 12 }
route21 = route11 { peerData = gd1Peer2 }
route22 = route12 { peerData = gd1Peer2 }
route23 = route13 { peerData = gd1Peer2 }
route31 = route11 { peerData = gd1Peer3 }
route32 = route12 { peerData = gd1Peer3 }
route33 = route13 { peerData = gd1Peer3 }

route peer pl = route11 { peerData = peer, pathLength = pl }
