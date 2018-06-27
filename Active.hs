{-# LANGUAGE OverloadedStrings #-}
-- active TCP speaker
module Active (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Common

main :: IO ()
main = do
    E.bracket open close talk
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock (SockAddrInet bgpPort ipV4_localhost)
        return sock
    talk sock = do
        sendAll sock "Hello, world!"
        msg <- recv sock 1024
        putStr "Received: "
        C.putStrLn msg
