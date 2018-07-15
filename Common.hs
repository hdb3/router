module Common(module Data.IP, module Common) where
import Data.IP
import Network.Socket (PortNumber)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
bgpPort = 179 :: PortNumber
ipV4_wildcard = toHostAddress ( read "0.0.0.0")
ipV4_localhost = toHostAddress ( read "127.0.0.1")

putn :: Binary b => [b] -> Put
putn pfxs | null pfxs =  return ()
          | otherwise =  do put (head pfxs)
                            putn ( tail pfxs)
getn :: Binary b => Get [b]

getn = do
    empty <- isEmpty
    if empty
    then return []
    else do b <- get
            bs <- getn
            return (b:bs)
