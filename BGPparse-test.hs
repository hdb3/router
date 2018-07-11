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

main = do identityCheck "BGPOpen" $ BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0]
          identityCheck "BGPKeepalive" BGPKeepalive
          identityCheck "BGPNotify - empty data" $ BGPNotify NotificationOPENMessageError (encode8 BadBGPIdentifier) []
          identityCheck "BGPNotify" $ BGPNotify  NotificationOPENMessageError (encode8 UnsupportedOptionalParameter) [ CapAS4 65520,  CapGracefulRestart False 0]
          identityCheck "BGPUpdate" $ BGPUpdate "Withdrawn routes" "Path Attributes" "nlri"

identityCheck :: String -> BGPMessage -> IO ()
identityCheck name bgpMsg = do putStrLn $ "identityCheck on " ++ name
                               let encMsg = encode bgpMsg :: L.ByteString
                               print $ simpleHex $ L.toStrict encMsg
                               let decMsg = decodeOrFail encMsg :: Either (L.ByteString, Int64, String) (L.ByteString, Int64, BGPMessage)
                               case decMsg of (Left (_,_,s)) -> do print $ "failed to decode message:" ++ s
                                                                   print $ simpleHex $ L.toStrict encMsg
                                              (Right ( _,_,decMsg')) -> if decMsg' == bgpMsg then do
                                                                                print "success"
                                                                                -- print $ simpleHex $ L.toStrict encMsg
                                                                        else do print "recoded message is not identical"
                                                                                print bgpMsg
                                                                                print $ simpleHex $ L.toStrict encMsg
                                                                                print decMsg'
                               putStrLn ""
