{-# LANGUAGE OverloadedStrings #-}
-- passive TCP server
module Main where

import Control.Concurrent (forkFinally,threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, send)
import Common
import BGPparse
import GetBGPMsg
import Data.Binary(encode,decode)
import System.Timeout
import Network.Socket.Options
import Data.Int(Int64)
import BgpFSM

holdTimer = 10 * 1000000 :: Int
holdTimer' = 10 * 1000000 :: Int64
seconds = 1000000 :: Int
keepAliveTimer = 2 * seconds
main :: IO ()
main = do
    putStrLn "Passive starting"
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
        void $ forkFinally (bgpFSMdelayOpen conn) (\_ -> close conn)
{-
    init sock = do 
                   sndBgpMessage sock $ encode $ BGPOpen 1000 600 65551 B.empty
                   msg <- getBgpMessage sock
                   let bgpMsg = decode msg :: BGPMessage
                   putStr "Init, Received: "
                   print bgpMsg
                   talk sock
    talk sock = do msg <- getBgpMessage' sock
                   let bgpMsg = decode msg :: BGPMessage
                   putStr "Received: "
                   print bgpMsg
                   threadDelay keepAliveTimer
                   sndBgpMessage sock $ encode $ BGPKeepalive
                   talk sock

    getBgpMessage' sock = do msg <- timeout holdTimer (getBgpMessage sock)
                             maybe (do print "timeout!"
                                       getBgpMessage' sock)
                                   (return)
                                   msg
-}
