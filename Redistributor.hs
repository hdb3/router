{-# LANGUAGE RecordWildCards #-}
module Redistributor where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(IOMode( ReadWriteMode ),Handle)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Control.Concurrent
import Control.Exception
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust,fromMaybe)
import Data.Either(either)
import qualified Data.Map.Strict as Data.Map

import BGPRib
import BGPlib
import Open
import Collision
import Route
import Global
import Config
import ZServ

redistribute :: Global -> IO ()
redistribute global@Global{..} =
    do threadId <- myThreadId
       putStrLn $ "Thread " ++ show threadId ++ " starting redistributor"
       ( zStreamIn, zStreamOut ) <- getZStreamUnix "/var/run/quagga/zserv.api"
       zservRegister zStreamOut _ZEBRA_ROUTE_BGP
       let dummyRouteInstaller (route, Nothing) = putStrLn $ "route not in Rib!: " ++ show route
           dummyRouteInstaller (route, Just nextHop) = do putStrLn $ "install " ++ show route ++ " via " ++ show nextHop
                                                          addRoute zStreamOut (toAddrRange $ toPrefix route) nextHop

        -- addPeer rib peerData
        -- delPeer rib peerData
        -- BGPRib.ribUpdater rib peerData parsedUpdate
       ribUpdateListener dummyRouteInstaller rib ( localPeer gd ) 1


ribUpdateListener routeInstaller rib peer timeout = do
    updates <- pullAllUpdates (1000000 * timeout) peer rib
    if null updates then
        yield -- null op - could check if exit from thread is needed...
    else do routes <- lookupRoutes rib peer updates
            putStr "Ready to install routes"
            if 11 > length updates then
                print $ map fst updates
            else do
                print $ map fst (take 10 updates)
                putStrLn $ "and " ++ show (length updates - 10) ++ " more"
            let getNextHop rib pfx = do nh <- lookupNextHop rib pfx
                                        return (pfx,nh)
            routes <- mapM ( getNextHop rib ) (concatMap fst updates)
            mapM_ routeInstaller routes

    -- rinse and repeat...

    ribUpdateListener routeInstaller rib peer timeout

