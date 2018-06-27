-- Echo server program
module Passive (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Common

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
    talk conn = do
        msg <- recv conn 1024
        unless (S.null msg) $ do
          sendAll conn msg
          talk conn
