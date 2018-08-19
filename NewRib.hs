{-# LANGUAGE RecordWildCards #-}
module NewRib where
import Data.IORef
import qualified Data.ByteString.Lazy as L

import BGPData
import Prefixes
import PrefixTable
import PathTable
import PathAttributes

{-
data Rib = Rib { prefixTable :: LinearHashTable Prefix Word64
                 , pathTable :: LinearHashTable Word64 ([PathAttribute],B.ByteString)
                 , pathTableRefCount :: LinearHashTable Word64 Word32
                 , as4 :: Bool
                 , updates :: IORef Int
                 , withdraws :: IORef Int
                }
instance Show Rib where show Rib{..} = if as4 then "AS4 Rib" else "AS2 Rib" 
summary :: Rib -> IO()
display :: Rib -> IO()
newRib :: IO Rib
newRib2 :: IO Rib
newRib4 :: IO Rib
ribUpdateMany :: Rib -> ([PathAttribute],L.ByteString)-> [Prefix] -> IO()
ribUpdate :: Rib -> ([PathAttribute],L.ByteString) -> Prefix -> IO()
ribWithdrawMany :: Rib -> [Prefix] -> IO()
ribWithdraw :: Rib -> Prefix -> IO()
-}
-- newtype Rib :: IORef Rib'
type Rib = IORef Rib'
data Rib' = Rib' { peer :: PeerData
                 , prefixTable :: PrefixTable
                 , pathTable :: PathTable
                 }

newRib :: IO Rib
newRib = newIORef newRib'
newRib' :: Rib'
newRib' = Rib' defaultPeerData newPrefixTable newPathTable

summary :: Rib -> IO String
summary rib = do
    rib' <- readIORef rib
    return $ showPrefixTable (prefixTable rib')

ribUpdateMany :: Rib -> [PathAttribute] -> Int -> [Prefix] -> IO()
ribUpdateMany rib attrs hash pfxs = modifyIORef' rib (ribUpdateMany' attrs hash pfxs)
ribUpdateMany' :: [PathAttribute] -> Int -> [Prefix] -> Rib' -> Rib'
ribUpdateMany' attrs hash pfxs (Rib' peerData prefixTable pathTable) = let
    ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
    -- pathTable' = pathTableInsert pathTable (length pfxs) routeData
    -- TODO - work out if the path table serves any usefull purpose...
    pathTable' = pathTable
    routeData = RouteData peerData attrs hash pathLength origin med fromEBGP
    fromEBGP = isExternal peerData
    pathLength = getASPathLength attrs
    med = if fromEBGP then 0 else getMED attrs
    origin = getOrigin attrs
    in Rib' peerData prefixTable' pathTable'

ribWithdrawMany rib p = modifyIORef' rib (ribWithdrawMany' p)
ribWithdrawMany' :: [Prefix] -> Rib' -> Rib'
ribWithdrawMany' pfxs (Rib' peerData prefixTable pathTable) = let
    ( prefixTable' , updates ) = PrefixTable.withdraw prefixTable (fromPrefixes pfxs) peerData
    -- todo - actually decrement the pathtable and delete - 
    --        requires the prefix table withdraw to return the routeData which was dropped....
    -- pathTable' = pathTableDelete pathTable (routeId routeData) (length pfxs)
    pathTable' = pathTable
    in Rib' peerData prefixTable' pathTable'
