module BgpFSM(bgpFSM) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Binary(encode,decode)
import Control.Concurrent
import Control.Monad(void)
import System.Timeout
import Hexdump
import Common
import BGPparse
import GetBGPMsg
import RFC4271

second = 1000000
minute = 60 * second
hour = 60 * minute
aLongTime = 1 * hour
keepAliveTimer = 5 * second
holdTimer = 15 * second
initialHoldTimer = 120 * second
delayOpenTimer = 120 * second

bgpFSM :: Socket -> IO ()
bgpFSM sock = stateConnected where
    snd msg = sndBgpMessage sock $ encode $ msg
    get' :: Int -> IO BGPMessage
    get' t = do mMsg <- timeout t (getBgpMessage sock)
                maybe
                    (return BGPTimeout)
                    (\msg -> do
                        let bgpMsg = decode msg :: BGPMessage
                        return bgpMsg)
                    -- (\msg -> return $ decode msg :: BGPMessage)
                    mMsg

    get = do msg <- getBgpMessage sock
             let bgpMsg = decode msg :: BGPMessage
             print bgpMsg
             return bgpMsg
    stateConnected = do msg <- get' delayOpenTimer
                        case msg of 
                            BGPTimeout -> do
                                putStrLn "stateConnected - delay open expiry"
                                snd $ BGPOpen 1000 600 65550 B.empty
                                putStrLn "transition -> stateOpenSent"
                                stateOpenSent
                            (BGPOpen a b c d) -> do
                                putStrLn "stateConnected - rcv open"
                                snd $ BGPOpen 1000 600 65550 B.empty
                                snd BGPKeepalive
                                putStrLn "transition -> stateOpenConfirm"
                                stateOpenConfirm
                            (BGPNotify a b c) -> exit "stateConnected - rcv notify"
                            otherwise -> do
                                snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                                exit "stateConnected - FSM error"
    stateOpenSent = do msg <- get' initialHoldTimer
                       case msg of 
                         BGPTimeout -> do
                             snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                             exit "stateOpenSent - error initial Hold Timer expiry"
                         (BGPOpen a b c d) -> do
                             putStrLn "stateOpenSent - rcv open"
                             snd BGPKeepalive
                             putStrLn "transition -> stateOpenConfirm"
                             stateOpenConfirm
                         otherwise -> do
                             snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                             exit "stateOpenConfirm - FSM error"
    stateOpenConfirm = do msg <- get' holdTimer
                          case msg of 
                              BGPTimeout -> do
                                  snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                                  exit "stateOpenSent - error initial Hold Timer expiry"
                              BGPKeepalive -> do
                                  putStrLn "stateOpenConfirm - rcv keepalive"
                                  toEstablished
                              (BGPNotify a b c) ->
                                  exit "stateOpenConfirm - rcv notify"
                              otherwise -> do
                                  snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                                  exit "stateOpenConfirm - FSM error"
    exit s = do putStrLn s
                fail s
    keepAliveLoop = do
        threadDelay keepAliveTimer
        snd BGPKeepalive
        keepAliveLoop
    toEstablished = do
        putStrLn "transition -> established"
        void $ forkIO keepAliveLoop
        established
    established = do
        msg <- get' holdTimer
        case msg of 
            BGPKeepalive ->
                do putStrLn "established - rcv keepalive"
                   established
            (BGPUpdate a b c) ->
                do putStrLn "established - rcv update"
                   established
            (BGPNotify a b c) -> exit "established - rcv notify"
            BGPTimeout -> do
                snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                exit "established - FSM error"
            otherwise -> do
                snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                exit "established - FSM error"
