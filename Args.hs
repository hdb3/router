module Args where
import System.Environment
import Control.Exception
import Data.List(intercalate)
import Network.Socket

import Common
import BgpFSM
import BGPparse
import Capabilities

type Args = (SockAddr, BGPMessage, BGPMessage)

-- here is an example of a valid parameter list:
-- "192.168.1.2,179" "30,40,192.168.1.1,CapAS4,65520,CapGracefulRestart,False,0" "0,0,127.0.0.1"
--
defaultParameterString = ["192.168.1.2,179","30,40,192.168.1.1,CapAS4,65520,CapGracefulRestart,False,0","0,0,127.0.0.1"]
helpMessage = "usage:  | address,port | address,port localBGPparameters | address,port localBGPparameters remoteBGPparameters \n\
               \where xBGPparameters = holdTime,AS,BGPID,optionalcapabilities\n\
               \zero values can be used to represent wildcards\n\
               \an example of this is:  " ++ intercalate "  " defaultParameterString

getConfig :: IO (Either String Args)
getConfig = do
    args <- getArgs
    
    -- this forces the parser to evaluate the command line inside the exception catcher rather than later on when 
    -- the client uses the parsed parameters
    let force v = if 0 < length  (show v) then return v else return undefined
    eVal <- try (force $ getConfig' args) :: IO (Either SomeException Args)

    return $ either
             (\_ -> Left helpMessage)
             -- (\e -> Left $ show e)
             -- expect "Prelude.read: no parse"
             Right
             eVal

getConfig' :: [String] -> Args
getConfig' args = (address,local,remote) where
    address =
        if length args > 0
        then
            parseAddress $ args !! 0
        else
            SockAddrInet bgpPort ipV4_localhost

    local =
        if length args > 1
        then
            parseParams $ args !! 1
        else
            BGPOpen 65500 40 (read "127.0.0.1") [ CapAS4 65500,  CapGracefulRestart False 0]

    remote =
        if length args > 2
        then
            parseParams $ args !! 2
        else
            BGPOpen 0 0 (read "0.0.0.0") []

    parseAddress :: String -> SockAddr
    parseAddress ps = SockAddrInet (read $ ws !! 1) (toHostAddress ip) where
                      ip = read $ ws !! 0
                      ws = myWords ps

    parseParams ps = BGPOpen (read $ ws !! 0) (read $ ws !! 1) (read $ ws !! 2) (parseCapabilities (drop 3 ws)) where
        ws = myWords ps

    parseCapabilities [] = []
    parseCapabilities ("CapAS4":as:cx) = CapAS4 (read as) : parseCapabilities cx
    parseCapabilities ("CapGracefulRestart":b:w:cx) = CapGracefulRestart (read b) (read w) : parseCapabilities cx
    parseCapabilities ("CapRouteRefresh":cx) = CapRouteRefresh : parseCapabilities cx
    parseCapabilities ("CapCiscoRefresh":cx) = CapCiscoRefresh : parseCapabilities cx

    myWords :: String -> [String]
    myWords "" = []
    myWords (',':ws) = myWords ws
    myWords ws = w : myWords ws' where
        (w,ws') = break (',' ==) ws
