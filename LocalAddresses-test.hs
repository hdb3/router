{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Network.Info -- package network-info
import Data.IP
import Control.Monad(liftM)
import LocalAddresses


main = do
    validAddresses <- getValidAddresses 
    putStrLn $ "valid local addresses: " ++ show validAddresses

    validAddress <- getValidAddress 
    putStrLn $ "valid local address: " ++ show validAddress

    validPublicAddress <- getPublicAddress 
    putStrLn $ "valid public address: " ++ show validPublicAddress

