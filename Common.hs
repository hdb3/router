
module Common where
import Network.Socket (tupleToHostAddress,PortNumber)
bgpPort = 179 :: PortNumber
-- these parameters are available in Network.IP.Addr which might be a better source 
ipV4_wildcard = tupleToHostAddress(0,0,0,0)
ipV4_localhost = tupleToHostAddress(127,0,0,1)
