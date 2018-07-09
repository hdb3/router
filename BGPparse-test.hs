{-# LANGUAGE OverloadedStrings #-}

{-
 unit test framework for the BGPMessage parser/deparser BGPparse.hs
-}

module Main where
import qualified Data.ByteString.Lazy as L
import BGPparse
import Data.Int(Int64)
import Hexdump
import Data.Binary
import RFC4271
import Capabilities

main = do identityCheck "BGPOpen" $ BGPOpen 1000 600 65550 "optional parameters"
          identityCheck "BGPKeepalive" BGPKeepalive
          identityCheck "BGPNotify" $ BGPNotify  NotificationOPENMessageError UnsupportedOptionalParameter [ CapAS4 65520,  CapGracefulRestart False 0]
          -- identityCheck "BGPNotify" $ BGPNotify 99 99 "Error data"
          identityCheck "BGPUpdate" $ BGPUpdate "Withdrawn routes" "Path Attributes" "nlri"

identityCheck :: String -> BGPMessage -> IO ()
identityCheck name bgpMsg = do putStrLn $ "identityCheck on " ++ name
                               let encMsg = encode bgpMsg :: L.ByteString
                               let decMsg = decodeOrFail encMsg :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, BGPMessage)
                               case decMsg of (Left (_,_,s)) -> do print $ "failed to decode message:" ++ s
                                                                   print $ simpleHex $ L.toStrict encMsg
                                              (Right ( _,_,decMsg')) -> if decMsg' == bgpMsg then print "success"
                                                                        else do print "recoded message is not identical"
                                                                                print bgpMsg
                                                                                print $ simpleHex $ L.toStrict encMsg
                                                                                print decMsg'
                               putStrLn ""
