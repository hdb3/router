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
import BgpFSM

second = 1000000
minute = 60 * second
hour = 60 * minute
aLongTime = 1 * hour

main :: IO ()
main = do
    E.bracket open close bgpFSM
  where
    open = do
        putStrLn "begin:: "
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        putStrLn "connected:: "
        return sock
{-
    talk sock = do
        sndBgpMessage sock $ encode $ BGPOpen 1000 600 65550 B.empty
        putStrLn "sent::open "
        msg <- getBgpMessage sock
        putStr "received:: "
        let bgpMsg = decode msg :: BGPMessage
        print bgpMsg
        sndBgpMessage sock $ encode BGPKeepalive
        threadDelay aLongTime
        talk sock
-}
