module Common(module Data.IP, module Common) where
import Data.IP
import Network.Socket (PortNumber)
bgpPort = 179 :: PortNumber
ipV4_wildcard = toHostAddress ( read "0.0.0.0")
ipV4_localhost = toHostAddress ( read "127.0.0.1")
