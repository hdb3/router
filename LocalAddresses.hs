{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LocalAddresses where
import Network.Info -- package network-info
import Data.IP
import Control.Monad(liftM)

acceptAddresses :: Network.Info.IPv4 -> Bool
acceptAddresses (Network.Info.IPv4 hostAddress) | ip4 `isMatchedTo` "127.0.0.0/8" = False
                                                | ip4 == "0.0.0.0" = False
                                                | ip4 == "169.254.169.254" = False
                                                | otherwise = True where ip4 = fromHostAddress hostAddress

publicAddresses = not . privateAddresses
privateAddresses :: Network.Info.IPv4 -> Bool
privateAddresses (Network.Info.IPv4 hostAddress) | ip4 `isMatchedTo` "10.0.0.0/8" = True
                                                 | ip4 `isMatchedTo` "172.16.0.0/12" = True
                                                 | ip4 `isMatchedTo` "192.168.0.0/16" = True
                                                 | otherwise = False where ip4 = fromHostAddress hostAddress

getValidAddress = liftM head getValidAddresses
getValidAddresses = do
    interfaces <- getNetworkInterfaces
    return $ filter acceptAddresses $ map Network.Info.ipv4 interfaces

getPrivateAddresses = liftM (filter privateAddresses) getValidAddresses
getPrivateAddress = liftM head getPrivateAddresses

getPublicAddresses = liftM (filter publicAddresses) getValidAddresses
getPublicAddress = liftM head getPublicAddresses
