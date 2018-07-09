module BgpFSM(bgpFSM,bgpFSMdelayOpen,bgpFSM') where
import Network.Socket
import qualified Data.ByteString as B
import Data.Binary(encode,decode)
import System.Timeout(timeout)
import Control.Concurrent(threadDelay,forkIO)
{-
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Control.Monad(void)
import Hexdump
-}
import Common
import BGPparse
import GetBGPMsg
--import RFC4271
import Open
import Capabilities

keepAliveTimer = 5
holdTimer = 15
initialHoldTimer = 120
defaultDelayOpenTimer = 20
bgpFSM :: OpenStateMachine -> Socket -> IO ()
bgpFSM osm sock = bgpFSM' osm sock 0
bgpFSMdelayOpen :: OpenStateMachine -> Socket -> IO ()
bgpFSMdelayOpen osm sock = bgpFSM' osm sock defaultDelayOpenTimer
bgpFSM' :: OpenStateMachine -> Socket -> Int -> IO ()
bgpFSM' osm sock delayOpenTimer = stateConnected osm where
    snd msg = sndBgpMessage sock $ encode msg
    get' :: Int -> IO BGPMessage
    get' t = let t' = t * 10000000 in
             do mMsg <- timeout t' (getBgpMessage sock)
                maybe
                    (return BGPTimeout)
                    (\msg -> do
                        let bgpMsg = decode msg :: BGPMessage
                        return bgpMsg)
                    mMsg

    get = do msg <- getBgpMessage sock
             let bgpMsg = decode msg :: BGPMessage
             print bgpMsg
             return bgpMsg

    stateConnected osm = do msg <- get' delayOpenTimer
                            case msg of 
                                BGPTimeout -> do
                                    putStrLn "stateConnected - delay open expiry"
                                    snd (localOffer osm)
                                    -- snd $ BGPOpen 1000 600 65550 B.empty
                                    putStrLn "transition -> stateOpenSent"
                                    stateOpenSent osm
                                open@(BGPOpen a b c d) -> do
                                    let osm' = updateOpenStateMachine osm open
                                    putStrLn "stateConnected - rcv open"
                                    print open
                                    let resp =  getResponse osm'
                                    if isKeepalive resp then do 
                                        putStrLn "transition -> stateOpenConfirm"
                                        snd (localOffer osm')
                                        stateOpenConfirm osm'
                                        snd resp
                                    else do
                                        snd resp
                                        exit "stateConnected - open rejected error"
                                notify@(BGPNotify a b c) -> do
                                   print notify
                                   exit "stateConnected - rcv notify"
                                _ -> do
                                    snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                                    exit "stateConnected - FSM error"

    stateOpenSent osm = do msg <- get' initialHoldTimer
                           case msg of 
                             BGPTimeout -> do
                                 snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                                 exit "stateOpenSent - error initial Hold Timer expiry"
                             open@(BGPOpen a b c d) -> do
                                 let osm' = updateOpenStateMachine osm open
                                 putStrLn "stateOpenSent - rcv open"
                                 print open
                                 let resp =  getResponse osm'
                                 snd resp
                                 if isKeepalive resp then do 
                                     putStrLn "transition -> stateOpenConfirm"
                                     stateOpenConfirm osm'
                                 else exit "stateOpenSent - open rejected error"
                             _ -> do
                                 snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                                 exit "stateOpenConfirm - FSM error"

    stateOpenConfirm osm = do msg <- get' holdTimer
                              case msg of 
                                  BGPTimeout -> do
                                      snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                                      exit "stateOpenSent - error initial Hold Timer expiry"
                                  BGPKeepalive -> do
                                      putStrLn "stateOpenConfirm - rcv keepalive"
                                      toEstablished osm'
                                  notify@(BGPNotify a b c) -> do
                                      print notify
                                      exit "stateOpenConfirm - rcv notify"
                                  _ -> do
                                      snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                                      exit "stateOpenConfirm - FSM error"

    exit s = do putStrLn s
                fail s

    keepAliveLoop = do
        threadDelay $ 1000000 * keepAliveTimer
        snd BGPKeepalive
        keepAliveLoop

    toEstablished osm = do
        putStrLn "transition -> established"
        forkIO keepAliveLoop
        -- void $ forkIO keepAliveLoop
        established osm

    established osm = do
        msg <- get' holdTimer
        case msg of 
            BGPKeepalive -> do
                putStrLn "established - rcv keepalive"
                established osm
            update@(BGPUpdate a b c) -> do
                putStrLn "established - rcv update"
                print update
                established osm
            notify@(BGPNotify a b c) -> do
                print notify
                exit "established - rcv notify"
            BGPTimeout -> do
                snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                exit "established - FSM error"
            _ -> do
                snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                exit "established - FSM error"
