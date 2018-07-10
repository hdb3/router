module Main where
-- active TCP speaker
import Control.Exception(bracket)
import Network.Socket
import Common
import BgpFSM
import BGPparse
import Capabilities

main :: IO ()
main = bracket open close (bgpFSM local remote)
  where
    local = BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0]
    remote = BGPOpen 65521 40 (read "192.168.0.2") [ CapAS4 65521,  CapGracefulRestart False 0]
    open = do
        putStrLn "begin:: "
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        putStrLn "connected:: "
        return sock
