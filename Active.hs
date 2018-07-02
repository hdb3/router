{-# LANGUAGE OverloadedStrings #-}
-- active TCP speaker
module Main where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Binary(encode,decode)
import Control.Concurrent
import Hexdump
import Common
import BGPparse
import GetBGPMsg

main :: IO ()
main = do
    E.bracket open close talk
  where
    open = do
        putStrLn "active:: "
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        putStrLn "connected:: "
        return sock
    talk sock = do
        putStrLn "talking:: "
        sndBgpMessage sock $ encode $ BGPOpen 1000 600 65550 B.empty
        putStrLn "Sent:: "
        msg <- getBgpMessage sock
        putStr "Received:: "
        print $ simpleHex $ L.toStrict msg
        putStrLn "----"
        let bgpMsg = decode msg :: BGPMessage
        print bgpMsg
        putStrLn ""
        sndBgpMessage sock $ encode BGPKeepalive
        threadDelay 5
        talk sock
