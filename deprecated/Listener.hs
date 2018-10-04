{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Data.Maybe
import Control.Concurrent
import Control.Monad (void,unless,when,forever)
import Network.Socket(getPeerName,PortNumber,SockAddr(..))
import qualified Network.Socket as NS
import Network.Simple.TCP
import System.IO
import Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error


bindSock' port ip = do
    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.setSocketOption sock NS.NoDelay 1
    NS.bind sock $ NS.SockAddrInet 179 0
    return sock

main = do
    lsock <- catchIOError
                 -- ( do (listeningSocket,_) <- bindSock HostIPv4 "179"
                 ( do listeningSocket <- bindSock' 179 0
                      return listeningSocket )
                 (\e -> do Errno errno <- getErrno
                           hPutStrLn stderr $ errReport' errno e
                           error "can't continue" )
    forever  ( accept lsock client )

client (sock,addr) = do
    hPutStrLn stderr $ "client connected to " ++ show addr
    closeSock sock

errReport' errno e = unlines
    [ "*** UNKNOWN exception, please record this"
    -- , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]
