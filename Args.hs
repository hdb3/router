module Args where
import Common
import BgpFSM
import BGPparse
import Capabilities
import Network.Socket

getConfig args = do
    let local = if length args > 0
        then
            parseParams $ args !! 0
        else
            BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0]
    let remote = if length args > 1
        then
            parseParams $ args !! 1
        else
            BGPOpen 0 0 (read "0.0.0.0") []
    print local
    print remote
    return (local,remote)

-- here is an example of a valid parameter list:
-- "30,40,192.168.1.1,CapAS4,65520,CapGracefulRestart,False,0"
--
parseAddress :: String -> SockAddr
parseAddress ps = SockAddrInet (read $ ws !! 1) (toHostAddress ip) where
                  ip = (read $ ws !! 0)
                  ws = myWords ps

parseParams ps = BGPOpen (read $ ws !! 0) (read $ ws !! 1) (read $ ws !! 2) (parseCapabilities (drop 3 ws)) where
    ws = myWords ps

parseCapabilities [] = []
parseCapabilities ("CapAS4":as:cx) = CapAS4 (read as) : parseCapabilities cx
parseCapabilities ("CapGracefulRestart":b:w:cx) = CapGracefulRestart (read b) (read w) : parseCapabilities cx

myWords :: String -> [String]
myWords "" = []
myWords (',':ws) = myWords ws
myWords ws = w : myWords ws' where
    (w,ws') = break (',' ==) ws
    notComma ',' = False
    notComma _ = True