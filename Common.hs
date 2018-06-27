
module Common where
import Network.Socket (tupleToHostAddress,PortNumber)
bgpPort = 179 :: PortNumber
ipV4_wildcard = tupleToHostAddress(0,0,0,0)
