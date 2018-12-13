{-# LANGUAGE RecordWildCards #-}
module Main where

import BGPReader(readMsgs)
-- import BGPlib
import BGPlib(Prefix,BGPMessage(..))
import Data.Binary

main = do
    msgs <- readMsgs
    putStrLn $ "got " ++ show (length msgs) ++ " messages"
    analyseMessageTypes msgs
    analysePrefixes msgs

analyseMessageTypes msgs = do
    let groups = foldl f ([],[],[],[]) msgs
        f (o,u,k,n) open@BGPOpen{..}           = (open:o,u,k,n)
        f (o,u,k,n) update@BGPUpdate{..}       = (o,update:u,k,n)
        f (o,u,k,n) keepalive@BGPKeepalive     = (o,u,keepalive:k,n)
        f (o,u,k,n) notification@BGPNotify{..} = (o,u,k,notification:n)
        count (o,u,k,n) = (length o,length u,length k,length n)
    putStrLn $ "count (o,u,k,n) = " ++ show (count groups)

analysePrefixes msgs = do
    let prefixes = foldl f (0,0) msgs
        prefixCount bs = length (decode bs :: [Prefix]) 
        f (u,w) BGPUpdate{..} = ( u + prefixCount nlri , w + prefixCount withdrawn )
        f (u,w) _ = (u,w)
    putStrLn $ "count (update,withdrawn) = " ++ show prefixes
