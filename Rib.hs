{-# LANGUAGE RecordWildCards #-}
module Rib where
import FarmHash(hash64)
import Data.HashTable.IO
import Control.Monad(when)
import Data.Word
import Data.Bits
import Data.List(foldl')
import Data.Maybe(fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Prefixes
import PathAttributes
import Count(count)

-- a RIB holds both infomation from updates and a little static context derived from the 
-- BGP Open exchange, of which the most significant is the AS4 capability which affects the way in which
-- AS Paths are stored
--
-- the module contains all of the access methods for using the RIB
-- as well as the data strucures themselves


-- this path table is naive - a faster one will store essential attributes explicitly,
-- e.g. path lnegth
--
-- additionally, since prefixes arrive in lists with a common path, it
-- would be far better to update the pathTable just once per block......
--

data Rib = Rib { prefixTable :: LinearHashTable Prefix Word64
                 , pathTable :: LinearHashTable Word64 ([PathAttribute],B.ByteString)
                 , pathTableRefCount :: LinearHashTable Word64 Word32
                 , as4 :: Bool }
instance Show Rib where show Rib{..} = if as4 then "AS4 Rib" else "AS2 Rib" 


print' = return
-- diagnotic to shorten the hash field only
-- myHash x = (hash64 x) .&. 0xfff
myHash = hash64

display Rib{..} = do
  s1 <- toList prefixTable
  s2 <- toList pathTable
  s3 <- toList pathTableRefCount
  return $ unlines [ "prefixTable", unlines $ map show s1
                   , "pathTable", unlines $ map show (map kv1 s2)
                   , "pathTableRefCount", unlines $ map show s3
                   , "as4", show $ as4 ]
  where kv1 (k,(v1,v2)) = (k,v1)

newRib :: IO Rib
newRib = do
    prefixTable <- (newSized 1000000)
    pathTable <- (newSized 100000)
    pathTableRefCount <- (newSized 100000)
    return $ Rib prefixTable pathTable pathTableRefCount False

newRib2 :: IO Rib
newRib2 = newRib
newRib4 :: IO Rib
newRib4 = do
    rib <- newRib
    return $ rib { as4 = True }

ribUpdateMany :: Rib -> ([PathAttribute],L.ByteString)-> [Prefix] -> IO()
ribUpdateMany Rib{..} (pathAttributes,bytes) prefixes = do
    oldNewHashMs <- mapM m' prefixes 
    let (oldHashMs,newHashMs) = foldl' (\(ax,bx) (a,b) -> (a:ax,b:bx)) ([],[]) oldNewHashMs
    print' newHashMs
    print' (count newHashMs)
    print' oldHashMs
    print' (count oldHashMs)
    Prelude.mapM_ updatePathTables (count newHashMs)
    Prelude.mapM_ withdrawPathTables (count oldHashMs)
    where
        bytes' = L.toStrict bytes
        hash = myHash bytes'
        m = flip ( mutate prefixTable)
        m' = m (\v -> (Just hash, (maybe 0 id v, hash)))
        withdrawPathTables (0,_) = return ()
        withdrawPathTables (hash,n) = do
            newRefCount <- mutate pathTableRefCount hash (f' n) 
            when (0 == newRefCount)
                 (do delete pathTable hash
                     delete pathTableRefCount hash )
            where
                    f' m Nothing = error "prefix is not present in ref count table"
                    f' m (Just refCount) = (Just (refCount-m), refCount-m)
        updatePathTables (hash,n) = do
            oldRefCount <- mutate pathTableRefCount hash (f' n) 
            when (0 == oldRefCount)
                (insert pathTable hash (pathAttributes,bytes')) where
                    f' m Nothing = (Just m, 0)
                    f' m (Just refCount) = (Just (refCount+m), refCount)


ribUpdate :: Rib -> ([PathAttribute],L.ByteString) -> Prefix -> IO()
ribUpdate Rib{..} (pathAttributes,bytes) prefix = do
    let bytes' = L.toStrict bytes
        hash = myHash bytes'
    oldHashM <- mutate prefixTable prefix (\v -> (Just hash, v))
    newRefCount <- mutate pathTableRefCount hash f' 
    when (1 == newRefCount)
        (insert pathTable hash (pathAttributes,bytes'))
    where
        f' Nothing = (Just 1, 1)
        f' (Just refCount) = (Just (refCount+1), refCount+1)

ribWithdrawMany :: Rib -> [Prefix] -> IO()
ribWithdrawMany Rib{..} prefixes = do
    let
        m = flip ( mutate prefixTable)
        m' = m (\v -> (Nothing, maybe 0 id v))
    oldHashMs <- mapM m' prefixes 
    Prelude.mapM_ withdrawPathTables (count oldHashMs)
    where
        withdrawPathTables (hash,n) = do
            newRefCount <- mutate pathTableRefCount hash (f' n) 
            when (0 == newRefCount)
                (do delete pathTable hash
                    delete pathTableRefCount hash ) where
                    f' m Nothing = error "hash is not present in ref count table"
                    f' m (Just refCount) = (Just (refCount-m), refCount-m)

ribWithdraw :: Rib -> Prefix -> IO()
ribWithdraw Rib{..} prefix = do
    oldPathHash <- mutate prefixTable prefix (\h -> (Nothing,fromJust h))
    newRefCount <- mutate pathTableRefCount oldPathHash f'
    when (0 == newRefCount)
         (do delete pathTable oldPathHash
             delete pathTableRefCount oldPathHash )
    where
        f' Nothing = error "hash is not present in ref count table"
        f' (Just 1) = (Nothing,0)
        f' (Just refCount) = (Just (refCount-1), refCount-1)
