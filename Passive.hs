-- passive TCP server
module Main where

import Control.Concurrent
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
    listen sock 100
    collisionDetector <- mkCollisionDetector
    exitMVar <- newEmptyMVar
    let config = BgpFSMconfig local remote undefined collisionDetector undefined delayOpenTimer exitMVar
    E.finally (loop sock config) (close sock)
  where
    delayOpenTimer = 10
    loop sock config = forever $ do
        (conn, peer) <- accept sock
        let config' = config { sock = conn, peerName = peer}
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (bgpFSM config') (\_ -> close conn)

        -- (tid,msg) <- takeMVar exitMVar
        -- putStrLn $ "complete:: " ++ show (tid :: ThreadId) ++ " : " ++ msg
