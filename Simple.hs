{-# LANGUAGE OverloadedStrings #-}
-- passive TCP server
module Simple (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, send)
import Common
import BGPparse
import Data.Binary(encode,decode)
import System.Timeout
import Network.Socket.Options
import Data.Int(Int64)

holdTimer = 3 * 1000000 :: Int64
holdTimer' = 10 * 1000000 :: Int
main :: IO ()
main = do sock <- socket AF_INET Stream defaultProtocol 
          setSocketOption sock ReuseAddr 1
          bind sock (SockAddrInet bgpPort ipV4_wildcard)
          listen sock 1
          (conn, peer) <- accept sock
          putStrLn $ "Connection from " ++ show peer
          setRecvTimeout conn holdTimer
          loop conn
        where
          timeout' t a = do Just z <- timeout t a
                            return z
          recv' s n = timeout' holdTimer' $ recv s n
          loop conn = do msg <- recv' conn 8192
                         let bgpMsg = decode msg :: BGPMessage
                         putStr "Received: "
                         print bgpMsg
                         loop conn
