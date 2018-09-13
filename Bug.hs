{-# LANGUAGE RecordWildCards #-}
{-#LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import qualified Data.IP
import Data.Char(toUpper)


import Data.Maybe
import Control.Concurrent
import Control.Monad (void,unless,when,forever)
import Network.Socket
import System.IO
import Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error


type App = (Network.Socket.Socket -> IO ())

logger = hPutStrLn stderr

seconds = 1000000
respawnDelay = 10 * seconds
idleDelay = 100 * seconds

session :: PortNumber -> App -> IO ()
session port app = do
    forkIO (listener port app )
    idle
    where
        idle = do
            threadDelay idleDelay
            idle


listener :: PortNumber -> App -> IO ()
listener port app = do
    logger "listener"
    listeningSocket <- socket AF_INET Stream defaultProtocol 
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port 0 )
    listen listeningSocket 100
    forever $ do
        (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
        logger $ "listener - connect request from " ++ show (fromHostAddress remoteIPv4)
        -- peerAddress <- getPeerName' sock
        -- let ip = fromPeerAddress peerAddress
        let ip = fromHostAddress remoteIPv4
        forkIO $ wrap app sock 
        close sock

fromPeerAddress (SockAddrInet _ ip) = fromHostAddress ip


wrap app sock = do
    peerAddress <- getPeerName' sock
    let ip = fromPeerAddress peerAddress
    catchIOError
        ( do logger $ "connected to : " ++ show ip
             app sock
             logger $ "app terminated for : " ++ show ip )
        (\e -> do Errno errno <- getErrno
                  logger $ "Exception in session with " ++ show ip ++ " - " ++ errReport errno e )
    close sock


getPeerName' sock = 
    catchIOError
        ( getPeerName sock )
        (\e -> do Errno errno <- getErrno
                  hPutStrLn stderr $ "Exception in getPeerName - " ++ errReport errno e 
                  return $ SockAddrInet 0 0 )


errReport 2 e = ioe_description e
errReport errno e = unlines
    [ "*** UNKNOWN exception, please record this"
    , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]



-- echo :: App
echo name sock = do
    putStrLn $ "echo starting with name " ++ name
    peerAddress  <- getPeerName sock
    localAddress <- getSocketName sock
    putStrLn $ "echo - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
    send sock "hello friend\n"
    reply <- recv sock 4096
    putStrLn $ "my friend said: \"" ++ reply ++ "\""
    send sock $ "you said " ++ (map toUpper reply)
    send sock "Goodbye!"
    return ()


main = do 
    session 5000 (echo "abc")
