module Main where
-- active TCP speaker
import Control.Exception(bracket)
import Network.Socket
import Common
import BgpFSM

main :: IO ()
main = bracket open close bgpFSM
  where
    open = do
        putStrLn "begin:: "
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        putStrLn "connected:: "
        return sock
