{-# LANGUAGE RecordWildCards #-}
module Session where

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
type RaceCheck = (IPv4 -> IO Bool)
type RaceCheckUnblock = (IPv4 -> IO ())
type Logger = (String -> IO ())
data State = State { port :: PortNumber
                   , logger :: Logger
                   , raceCheckBlock :: RaceCheck
                   , raceCheckNonBlock :: RaceCheck
                   , raceCheckUnblock :: RaceCheckUnblock
                   }

seconds = 1000000
respawnDelay = 10 * seconds
idleDelay = 100 * seconds

mkState port = do
    logger <- getLogger
    mapMVar <- newMVar Data.Map.empty
    let raceCheckNonBlock = raceCheck False mapMVar
        raceCheckBlock = raceCheck True mapMVar
        raceCheckUnblock = raceCheckUnblocker mapMVar
    return State {..}

getLogger = do
    let logThread mvar = do
        takeMVar mvar >>= hPutStrLn stderr
        logThread mvar
    logMVar <- newEmptyMVar
    forkIO ( logThread logMVar )
    return (putMVar logMVar)

-- RACE CHECK
-- before calling the application perform a race check
-- if there is a race then don't call the application
-- optionally, block waiting for the other session to complete
-- the race check has three entry points - blocking and non block request, and unblock.
-- the race check uses a map stored in MVar, and another MVar to support the blocking request
-- the blocked request returns an error condition even when unblocked, to prevent an
-- overeager talker from sharing the limelight too easily

raceCheckUnblocker :: MVar (Data.Map.Map IPv4 (MVar ())) -> IPv4 -> IO ()
raceCheckUnblocker mapMVar address = do
    map <- readMVar mapMVar
    let Just peerMVar = Data.Map.lookup address map
    putMVar peerMVar ()

raceCheck :: Bool -> MVar (Data.Map.Map IPv4 (MVar ())) -> IPv4 -> IO Bool
raceCheck blocking mapMVar address = do
-- get the specific MVar out of the Map
-- if it doesn't exist then insert it and take it
-- this is non-blocking so if it does exist but is empty then just exit 
    -- threadId <- myThreadId
    map <- takeMVar mapMVar
    let maybePeerMVar = Data.Map.lookup address map
    maybe (do peerMVar <- newEmptyMVar :: IO (MVar ())
              putMVar mapMVar (Data.Map.insert address peerMVar map)
              return True )
          (\peerMVar -> do
              putMVar mapMVar map
              maybeFree <- tryTakeMVar peerMVar
              if isJust maybeFree
              then return True
              else if not blocking
              then return False else
                  do
                  _ <- readMVar peerMVar
                  return False
          )
          maybePeerMVar

session :: PortNumber -> Maybe App -> [Peer] -> IO ()
session port defaultApp peers = do
-- TODO make this a monad to hide the logger plumbing
    state <- mkState port
    forkIO (listener state peers defaultApp)
    -- threads <- mapM ( forkIO . run state ) peers
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
        unblocked <- raceCheckNonBlock ip
        if not unblocked then do
            logger $ "listener - connect reject due to race"
            close sock
        else do
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
            raceCheckUnblock ip

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


run :: State -> Peer -> IO ()
run state@State{..} (ip,app) = do
    unblocked <- raceCheckBlock ip
    when unblocked
         ( do sock <- connectTo port ip
              maybe ( return () )
                    (wrap state app)
                    sock
              raceCheckUnblock ip )
    unless unblocked ( logger $ "run blocked for " ++ show ip )
    threadDelay respawnDelay
    run state (ip,app)
    where
    connectTo port ip =
        catchIOError
        ( do sock <- socket AF_INET Stream defaultProtocol
             Network.Socket.connect sock ( SockAddrInet port (toHostAddress ip))
             return $ Just sock )
        (\e -> do
            Errno errno <- getErrno
            logger $ "Exception connecting to " ++ show ip ++ " - " ++ errReport errno e
            return Nothing )


errReport 2 e = ioe_description e
errReport errno e = unlines
    [ "*** UNKNOWN exception, please record this"
    , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]
