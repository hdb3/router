{-# LANGUAGE OverloadedStrings #-}
-- Echo server program
module Passive (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Common
import BGPparse
import Data.Binary(encode,decode)

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
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk sock = do
        msg <- recv sock 8192
        unless (L.null msg) $ do
            let bgpMsg = decode msg :: BGPMessage
            putStr "Received: "
            print bgpMsg
            sendAll sock $ encode $ BGPOpen 1000 600 65551 B.empty
            talk sock


