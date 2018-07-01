{-# LANGUAGE OverloadedStrings #-}
-- passive TCP server
module Passive (main) where

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

holdTimer = 10 * 1000000 :: Int
holdTimer' = 10 * 1000000 :: Int64
main :: IO ()
main = do
    E.bracket open close loop
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol 
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet bgpPort ipV4_wildcard)
        listen sock 10
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        -- do setRecvTimeout conn holdTimer' -- DOESN'T WORK!!!!
        void $ forkFinally (talk conn) (\_ -> close conn)
    noOp sock = do print "timeout!"
                   talk sock
    talk sock = do msg <- timeout 100 (recv sock 8192)
                   maybe (noOp sock) (talk' sock) msg
    talk' sock msg = unless (L.null msg) $ do
                     let bgpMsg = decode msg :: BGPMessage
                     putStr "Received: "
                     print bgpMsg
                     send sock $ encode $ BGPOpen 1000 600 65551 B.empty
                     send sock $ encode $ BGPKeepalive
                     talk sock
