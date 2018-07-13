module Main where
-- active TCP speaker
import System.Environment
import Control.Exception(finally)
import Control.Concurrent
import Network.Socket
import Common
import BgpFSM
import BGPparse
import Capabilities
import Args
import Collision

main :: IO ()
main = do config <- getConfig
          either putStrLn
                 main'
                 config

main' (address,local,remote) = do
    print (address,local,remote)
    putStrLn "begin:: "
    sock <- socket AF_INET Stream defaultProtocol
    connect sock address
    putStrLn "connected:: "
    collisionDetector <- mkCollisionDetector
    peerName <- getPeerName sock
    let delayOpenTimer = 0
    exitMVar <- newEmptyMVar
    let config = BgpFSMconfig local remote sock collisionDetector peerName delayOpenTimer exitMVar
    finally (bgpFSM config) (close sock) 
    (tid,msg) <- takeMVar exitMVar
    putStrLn $ "complete:: " ++ show (tid :: ThreadId) ++ " : " ++ msg
