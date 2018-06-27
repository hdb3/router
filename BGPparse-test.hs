module Main where
import BGPparse
import Hexdump
import Data.Binary
import Data.Int
import Data.ByteString.Lazy
import qualified Data.ByteString as B

main = do
          let openmsg = BGPOpen 1000 600 65550
          print openmsg
          let encMsg = encode openmsg :: ByteString
          print $ simpleHex $ toStrict encMsg
          -- let decMsg = decode encMsg :: BGPMessage
          -- print decMsg

          let decMsg = decodeOrFail encMsg :: Either (ByteString, Int64, String) (ByteString, Int64, BGPMessage)
          case decMsg of (Left (_,_,s)) -> do print $ "failed to decode message:" -- ++ s :: String
                         (Right ( _,_,decMsg')) -> do print decMsg'

-- toStrict lbs = B.concat $ toChunks lbs
