{-# LANGUAGE OverloadedStrings #-}
-- active TCP speaker
module Active (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, send)
import Data.Binary(encode,decode)
import Control.Concurrent
import Common
import BGPparse

main :: IO ()
main = do
    E.bracket open close talk
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        return sock
    talk sock = do
        send sock $ encode $ BGPOpen 1000 600 65550 B.empty
        msg <- recv sock 8192
        let bgpMsg = decode msg :: BGPMessage
        putStr "Received: "
        print bgpMsg
        putStrLn ""
        send sock $ encode BGPKeepalive
        threadDelay 5
        talk sock
