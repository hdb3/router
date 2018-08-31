module Args2 where
import System.Environment
import Control.Exception
import Data.List(intercalate)
import Data.Word
import Data.IP
-- import Network.Socket

import Common
import BgpFSM
import BGPparse
import Capabilities
import BGPData
import LocalAddresses

defaultParameterString = ["192.168.1.2,179","30,40,192.168.1.1,CapAS4,65520,CapGracefulRestart,False,0","0,0,127.0.0.1"]
helpMessage = "usage:  | address,port | address,port localBGPparameters | address,port localBGPparameters remoteBGPparameters \n\
               \where xBGPparameters = holdTime,AS,BGPID,optionalcapabilities\n\
               \zero values can be used to represent wildcards\n\
               \an example of this is:  " ++ intercalate "  " defaultParameterString

getConfig :: IO (Either String [PeerData])
getConfig = do
    args <- getArgs
    localIPv4 <- getBestAddress
    
    -- this forces the parser to evaluate the command line inside the exception catcher rather than later on when 
    -- the client uses the parsed parameters
    let force v = if 0 < length  (show v) then return v else return undefined
    eVal <- try (force $ getConfig' localIPv4 args) :: IO (Either SomeException [PeerData])

    return $ either
             (\_ -> Left helpMessage)
             -- (\e -> Left $ show e)
             -- expect "Prelude.read: no parse"
             Right
             eVal

getConfig' :: IPv4 -> [String] -> [PeerData]
getConfig' localIPv4 args = pds where

    offerCapabilies = [ CapAS4 myAS ]
    localPref = 0

    (myAS, myBGPid, myHoldTime) = parseLocalParams (head args)

    gd = GlobalData myAS myBGPid
    pd = PeerData gd undefined undefined undefined undefined localIPv4 localPref myHoldTime 0 offerCapabilies []
    pds = map (updatePeer pd) (tail args)


updatePeer :: PeerData -> String -> PeerData
updatePeer pd arg = updatePeer' pd (parseRemoteParams arg)
    where
    updatePeer' :: PeerData -> (Word32 , IPv4 , [Capability] ) -> PeerData
    updatePeer' pd (remoteAS, remoteIPv4, remoteCaps) = let external = remoteAS /= (myAS $ globalData pd) in
                                                        pd { peerAS = remoteAS
                                                             , peerBGPid = remoteIPv4
                                                             , peerIPv4 = remoteIPv4
                                                             , requireCapabilies = remoteCaps
                                                             , isExternal = external
                                                            }

parseLocalParams  ps = let ws = myWords ps in (read $ ws !! 0,read $ ws !! 1,read $ ws !! 2)
parseRemoteParams ps = let ws = myWords ps in (read $ ws !! 0,read $ ws !! 1,parseCapabilities (drop 2 ws))
myWords :: String -> [String]
myWords "" = []
myWords (',':ws) = myWords ws
myWords ws = w : myWords ws' where
    (w,ws') = break (',' ==) ws

parseCapabilities [] = []
parseCapabilities ("CapAS4":as:cx) = CapAS4 (read as) : parseCapabilities cx
parseCapabilities ("CapGracefulRestart":b:w:cx) = CapGracefulRestart (read b) (read w) : parseCapabilities cx
parseCapabilities ("CapRouteRefresh":cx) = CapRouteRefresh : parseCapabilities cx
parseCapabilities ("CapCiscoRefresh":cx) = CapCiscoRefresh : parseCapabilities cx