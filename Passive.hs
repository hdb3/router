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
import Collision
import Args

main :: IO ()
main = do
    (address,local,remote) <- getConfig
    putStrLn "Passive starting"
    sock <- socket AF_INET Stream defaultProtocol 
    setSocketOption sock ReuseAddr 1
    bind sock address
    listen sock 10
    cd <- mkCollisionDetector
    E.finally (loop local remote sock cd) (close sock)
  where
    loop local remote sock cd = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (bgpFSMdelayOpen local remote conn cd) (\_ -> close conn)
