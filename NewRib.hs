{-# LANGUAGE RecordWildCards #-}
module Rib where
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
ribUpdateMany :: Rib -> ([PathAttribute],L.ByteString)-> [Prefix] -> IO()
ribUpdateMany rib t p = modifyIORef' rib (ribUpdateMany' t p)
ribUpdateMany' :: ([PathAttribute],L.ByteString)-> [Prefix] -> Rib' -> Rib'
ribUpdateMany' (attrs,bs) pfxs (Rib' peerData prefixTable pathTable) = let
    ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
    ( routeId , pathTable' ) = pathTableInsert pathTable (attrs,L.toStrict bs) (length pfxs) routeData
    routeData = RouteData peerData pathLength origin med fromEBGP
    fromEBGP = isExternal peerData
    pathLength = getASPathLength attrs
    med = if fromEBGP then 0 else getMED attrs
    origin = getOrigin attrs
    in Rib' peerData prefixTable' pathTable'
-- ribWithdrawMany' :: [Prefix] -> Rib -> Rib
