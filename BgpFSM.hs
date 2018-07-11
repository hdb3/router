module BgpFSM(bgpFSM,bgpFSMdelayOpen,bgpFSM') where
import Network.Socket
import qualified Data.ByteString as B
import Data.Binary(encode,decode)
import System.Timeout(timeout)
import Control.Concurrent(threadDelay,forkIO)
import Common
import BGPparse
import GetBGPMsg
import RFC4271
import Open
import Capabilities
import Collision

keepAliveTimer = 5
holdTimer = 15
initialHoldTimer = 120
defaultDelayOpenTimer = 20
bgpFSM :: BGPMessage -> BGPMessage -> Socket -> CollisionDetector-> IO ()
bgpFSM local remote sock cd = bgpFSM' local remote sock cd 0
bgpFSMdelayOpen :: BGPMessage -> BGPMessage -> Socket -> CollisionDetector -> IO ()
bgpFSMdelayOpen local remote sock cd = bgpFSM' local remote sock cd defaultDelayOpenTimer
bgpFSM' :: BGPMessage -> BGPMessage -> Socket -> CollisionDetector -> Int -> IO ()
bgpFSM' local remote sock cd delayOpenTimer = stateConnected osm where
    osm = makeOpenStateMachine local remote cd
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
                                    putStrLn "stateConnected - event: delay open expiry"
                                    snd (localOffer osm)
                                    putStrLn "stateConnected -> stateOpenSent"
                                    stateOpenSent osm
                                open@BGPOpen{} -> do
                                    let osm' = updateOpenStateMachine osm open
                                    putStrLn "stateConnected - event: rcv open"
                                    print open
                                    let resp =  getResponse osm'
                                    collision <- collisionCheck osm cd
                                    if not isKeepalive collision then do 
                                        snd collision
                                        exit "stateConnected - event: collision - open rejected error"
                                    else if isKeepalive resp then do 
                                        putStrLn "stateConnected -> stateOpenConfirm"
                                        snd (localOffer osm')
                                        stateOpenConfirm osm'
                                        snd resp
                                    else do
                                        snd resp
                                        exit "stateConnected - event: open rejected error"
                                notify@BGPNotify{} -> do
                                   print notify
                                   exit "stateConnected -> exit rcv notify"
                                _ -> do
                                    snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                                    exit "stateConnected - FSM error"

    stateOpenSent osm = do msg <- get' initialHoldTimer
                           case msg of 
                             BGPTimeout -> do
                                 snd $ BGPNotify NotificationHoldTimerExpired 0 []
                                 exit "stateOpenSent - error initial Hold Timer expiry"
                             open@BGPOpen{} -> do
                                 let osm' = updateOpenStateMachine osm open
                                 putStrLn "stateOpenSent - rcv open"
                                 print open
                                 let resp =  getResponse osm'
                                 snd resp
                                 if isKeepalive resp then do 
                                     putStrLn "stateOpenSent -> stateOpenConfirm"
                                     stateOpenConfirm osm'
                                 else exit "stateOpenSent - open rejected error"
                             notify@BGPNotify{} -> do
                                print notify
                                exit "stateOpenSent - rcv notify"
                             _ -> do
                                 snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                                 exit "stateOpenSent - FSM error"

    stateOpenConfirm osm = do msg <- get' holdTimer
                              case msg of 
                                  BGPTimeout -> do
                                      snd $ BGPNotify NotificationHoldTimerExpired 0 []
                                      exit "stateOpenConfirm - error initial Hold Timer expiry"
                                  BGPKeepalive -> do
                                      putStrLn "stateOpenConfirm - rcv keepalive"
                                      toEstablished osm
                                  notify@BGPNotify{} -> do
                                      print notify
                                      exit "stateOpenConfirm - rcv notify"
                                  _ -> do
                                      snd $ BGPNotify NotificationFiniteStateMachineError 0 []
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
            update@BGPUpdate{} -> do
                putStrLn "established - rcv update"
                print update
                established osm
            notify@BGPNotify{} -> do
                print notify
                exit "established - rcv notify"
            BGPTimeout -> do
                snd $ BGPNotify NotificationHoldTimerExpired 0 []
                exit "established - FSM error"
            _ -> do
                snd $ BGPNotify NotificationFiniteStateMachineError 0 []
                exit "established - FSM error"
