-- passive TCP server
module Main where

import Control.Concurrent (forkFinally,threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, send)
import Common
import BGPparse
import GetBGPMsg
import Data.Binary(encode,decode)
import System.Timeout
import Data.Int(Int64)
import BgpFSM
import Capabilities

main :: IO ()
main = do
    putStrLn "Passive starting"
    E.bracket open close loop
  where
    remote = BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0]
    local = BGPOpen 65521 40 (read "192.168.0.2") [ CapAS4 65521,  CapGracefulRestart False 0]
    bgpFSM = bgpFSMdelayOpen local remote
    open = do
        sock <- socket AF_INET Stream defaultProtocol 
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet bgpPort ipV4_wildcard)
        listen sock 10
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (bgpFSM conn) (\_ -> close conn)
