{-# LANGUAGE RecordWildCards #-}
module Session where

import Data.Maybe
import Control.Concurrent
import Control.Monad (void,unless,when,forever)
import Network.Socket(getPeerName,PortNumber,SockAddr(..))
import Network.Simple.TCP
import System.IO
import Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error


type App = (Socket -> IO ())
type RaceCheck = (IPv4 -> IO Bool)
type RaceCheckUnblock = (IPv4 -> IO ())
type Logger = (String -> IO ())
data State = State { port :: PortNumber
                   , logger :: Logger
                   , raceCheckBlock :: RaceCheck
                   , raceCheckNonBlock :: RaceCheck
                   , raceCheckUnblock :: RaceCheckUnblock
                   , defaultApp :: App
                   , peers :: [IPv4]
                   }

seconds = 1000000
respawnDelay = 10 * seconds
idleDelay = 100 * seconds

mkState port defaultApp peers = do
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

session :: PortNumber -> App -> [IPv4] -> IO ()
session port defaultApp peers = do
-- TODO make this a monad to hide the logger plumbing
    state <- mkState port defaultApp peers
    forkIO (listener state )
    threads <- mapM ( forkIO . run state ) peers
    idle
    where
        idle = do
            threadDelay idleDelay
            idle

listener :: State -> IO ()
listener state@State{..} = do
    logger "listener"
    (listeningSocket,_) <- bindSock HostIPv4 (show port )
    listenSock listeningSocket 100
    forever $ do
        accept listeningSocket ( listenClient state )

listenClient state@State{..} (sock, SockAddrInet remotePort remoteIPv4) = do
        let ip = fromHostAddress remoteIPv4
        logger $ "listener - connect request from " ++ show ip
        unblocked <- raceCheckNonBlock ip
        if not unblocked then do
            logger $ "listener - connect reject due to race"
            closeSock sock
        else do
            forkIO $ wrap state defaultApp sock 
            raceCheckUnblock ip

fromPeerAddress (SockAddrInet _ ip) = fromHostAddress ip


wrap state@State{..} app sock = do
    peerAddress <- getPeerName sock
    let ip = fromPeerAddress peerAddress
    catchIOError
        ( do logger $ "connected to : " ++ show ip
             app sock
             closeSock sock
             logger $ "app terminated for : " ++ show ip )
        (\e -> do Errno errno <- getErrno
                  logger $ "Exception in session with " ++ show ip ++ " - " ++ errReport errno e )

run :: State -> IPv4 -> IO ()
run state@State{..} ip = do
    unblocked <- raceCheckBlock ip
    when unblocked
         ( do sock <- connectTo port ip
              maybe ( return () )
                    (wrap state defaultApp)
                    sock
              raceCheckUnblock ip )
    unless unblocked ( logger $ "run blocked for " ++ show ip )
    threadDelay respawnDelay
    run state ip
    where
    connectTo port ip =
        catchIOError
        ( do (sock,_) <- connectSock (show ip) (show port)
             return $ Just sock )
        (\e -> do
            Errno errno <- getErrno
            logger $ "Exception connecting to " ++ show ip ++ " - " ++ errReport errno e
            return Nothing )


errReport errno e | errno `elem` [2,107] = ioe_description e ++ " (" ++ show errno ++ ")"
                  | otherwise = unlines
    [ "*** UNKNOWN exception, please record this"
    , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]
