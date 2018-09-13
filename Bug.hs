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
type Peer = (IPv4, App )
type Logger = (String -> IO ())
data State = State { port :: PortNumber
                   , logger :: Logger
                   }

seconds = 1000000
respawnDelay = 10 * seconds
idleDelay = 100 * seconds

mkState port = do
    logger <- getLogger
    mapMVar <- newMVar Data.Map.empty
    return State {..}

getLogger = do
    let logThread mvar = do
        takeMVar mvar >>= hPutStrLn stderr
        logThread mvar
    logMVar <- newEmptyMVar
    forkIO ( logThread logMVar )
    return (putMVar logMVar)

session :: PortNumber -> Maybe App -> [Peer] -> IO ()
session port defaultApp peers = do
-- TODO make this a monad to hide the logger plumbing
    state <- mkState port
    forkIO (listener state peers defaultApp)
    idle
    where
        idle = do
            threadDelay idleDelay
            idle


listener :: State -> [Peer] -> Maybe App -> IO ()
listener state@State{..} apps defaultApp = do
    logger "listener"
    let peerMap = Data.Map.fromList apps
    listeningSocket <- socket AF_INET Stream defaultProtocol 
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port 0 )
    listen listeningSocket 100
    forever $ do
        (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
        logger $ "listener - connect request from " ++ show (fromHostAddress remoteIPv4)
        -- threadDelay respawnDelay
        -- peerAddress <- getPeerName' sock
        -- let ip = fromPeerAddress peerAddress
        let ip = fromHostAddress remoteIPv4
        -- lookup may return Nothing, in which case thedefaultApp is used, unless that is nothing....
        -- NOTE - arguably this is more complex than it need be - the defauly app could be non-optional
        -- but simply close the socket itself, as this does.
        let runnable = fromMaybe
                (\_ -> logger "no default application given")
                ( maybe defaultApp
                        Just
                        ( Data.Map.lookup (fromHostAddress remoteIPv4) peerMap ))
        forkIO $ wrap state runnable sock 
        close sock

fromPeerAddress (SockAddrInet _ ip) = fromHostAddress ip


-- wrap :: State -> App -> Either Network.Socket.Socket SockAddr -> IO ()
wrap state@State{..} app sock = do
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
    let peers = map (\(a,b) -> (a, echo b))  
            [ ("192.168.122.236" , "yin")
            -- , ("192.168.122.60" , "yang")
            , ("192.168.122.178" , "yung")
            ]
    session 5000 Nothing [("192.168.122.236", echo "abc")]
    -- session 5000 (Just (echo "default app")) peers
