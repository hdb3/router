module Main where
-- active TCP speaker
import System.Environment
import Control.Exception(finally)
import Network.Socket
import Common
import BgpFSM
import BGPparse
import Capabilities
import Args
import Collision

main :: IO ()
main = do
        (address,local,remote) <- getConfig
        putStrLn "begin:: "
        sock <- socket AF_INET Stream defaultProtocol
        connect sock address
        putStrLn "connected:: "
        cd <- mkCollisionDetector
        finally (bgpFSM local remote sock cd) (close sock) 
        putStrLn "complete:: "
