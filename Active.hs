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
    local = BGPOpen 1234 40 65520 [ CapAS4 65520,  CapGracefulRestart False 0]
    remote = BGPOpen 4321 40 65521 [ CapAS4 65520,  CapGracefulRestart False 0]
    open = do
        putStrLn "begin:: "
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        putStrLn "connected:: "
        return sock
