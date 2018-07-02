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
import RFC4271

keepAliveTimer = 5
holdTimer = 15
initialHoldTimer = 120
defaultDelayOpenTimer = 20
bgpFSM :: Socket -> IO ()
bgpFSM sock = bgpFSM' sock 0
bgpFSMdelayOpen :: Socket -> IO ()
bgpFSMdelayOpen sock = bgpFSM' sock defaultDelayOpenTimer
bgpFSM' :: Socket -> Int -> IO ()
bgpFSM' sock delayOpenTimer = stateConnected where
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
    stateConnected = do msg <- get' delayOpenTimer
                        case msg of 
                            BGPTimeout -> do
                                putStrLn "stateConnected - delay open expiry"
                                snd $ BGPOpen 1000 600 65550 B.empty
                                putStrLn "transition -> stateOpenSent"
                                stateOpenSent
                            open@(BGPOpen a b c d) -> do
                                putStrLn "stateConnected - rcv open"
                                print open
                                snd $ BGPOpen 1000 600 65550 B.empty
                                snd BGPKeepalive
                                putStrLn "transition -> stateOpenConfirm"
                                stateOpenConfirm
                            notify@(BGPNotify a b c) -> do
                               print notify
                               exit "stateConnected - rcv notify"
                            _ -> do
                                snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                                exit "stateConnected - FSM error"
    stateOpenSent = do msg <- get' initialHoldTimer
                       case msg of 
                         BGPTimeout -> do
                             snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                             exit "stateOpenSent - error initial Hold Timer expiry"
                         open@(BGPOpen a b c d) -> do
                             putStrLn "stateOpenSent - rcv open"
                             print open
                             snd BGPKeepalive
                             putStrLn "transition -> stateOpenConfirm"
                             stateOpenConfirm
                         _ -> do
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
    toEstablished = do
        putStrLn "transition -> established"
        forkIO keepAliveLoop
        -- void $ forkIO keepAliveLoop
        established
    established = do
        msg <- get' holdTimer
        case msg of 
            BGPKeepalive -> do
                putStrLn "established - rcv keepalive"
                established
            update@(BGPUpdate a b c) -> do
                putStrLn "established - rcv update"
                print update
                established
            notify@(BGPNotify a b c) -> do
                print notify
                exit "established - rcv notify"
            BGPTimeout -> do
                snd $ BGPNotify _Notification_Hold_Timer_Expired 0 B.empty
                exit "established - FSM error"
            _ -> do
                snd $ BGPNotify _Notification_Finite_State_Machine_Error 0 B.empty
                exit "established - FSM error"
