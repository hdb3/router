{-#LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import qualified Data.IP
import Session
import Data.Char(toUpper)

-- echo :: App
echo name sock = do
    putStrLn $ "echo starting with name " ++ name
    peerAddress  <- getPeerName sock
    localAddress <- getSocketName sock
    putStrLn $ "echo - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
    send sock "hello friend\n"
    reply <- recv sock 4096
    putStrLn $ "my friend said: \"" ++ reply ++ "\""
    send sock $ "you said " ++ (map toUpper reply)
    send sock "Goodbye!"
    return ()


main = do 
    -- session 5000 [("192.168.122.179" , echo)]
    -- session 5000 echo [("192.168.122.179" , echo) , ("192.168.122.113" , echo) , ("192.168.122.178" , echo)]
    let peers = map (\(a,b) -> (a, echo b))  
            [ ("192.168.122.236" , "yin")
            , ("192.168.122.60" , "yang")
            , ("192.168.122.178" , "yung")
            ]
    session 5000 Nothing peers
    -- session 5000 (Just (echo "default app")) peers
