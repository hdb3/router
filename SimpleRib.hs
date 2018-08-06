{-# LANGUAGE RecordWildCards #-}
module SimpleRib where
import FarmHash(hash64)
import Data.HashTable.IO
import Control.Monad(when,unless)
import Data.Word
import Data.Bits
import Data.IORef
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
{-
-- These actions require operations on the various RIBs.  Changing the implementation of the RIBs should not change the interface.  These are the operations:
AdjRIB actions:
- PathTable insert: provide the prefix count so ref count can be maintained.  This also updates the ref count for the dereferenced paths.   Unreferenced paths are removed.  A path table reference is returned.
- PathTable delete: provide the prefix count as for insert.  Otherwise as insert, except that the PathTable is referenced by ID.
- Prefix table insert: provide a prefix and a path table reference. Returns a path table reference or null.
- Prefix table delete: Returns a path table reference or null.
- Peer delete - return a list of ([prefix]/path) tuples and deallocate the AdjRIB.
LocRIB (PathSelection) operations:
- Start update (bracket)
- proposeRoute - provide prefix and path costs and path reference.  An update may be scheduled as a result
- withdrawRoute - provide prefix and path reference.  An update may be scheduled as a result
- End update (bracket)
Notes
- these actions may (probably do) overlap
- A path preference calculation is required, but it is not part of the RIB update interface.  It requires a datatype which is a member of Ord
- A path update calculation is also required. This takes the proposed path and generates an updated version for dissemination.  It could be done for each path proposed, though it is not necessary until chosen (lazy is good ;-) ).
- The actual interfaces are block based for multiple prefixes..... This should allow
- There are also (obvs) object creation methods.  These should be iniialised with the calculation functions

-}

type MyHashTable = BasicHashTable
-- type MyHashTable = CuckooHashTable
-- type MyHashTable = LinearHashTable

data Rib = Rib { prefixTable :: MyHashTable Prefix Word64
                 , pathTable :: MyHashTable Word64 ([PathAttribute],B.ByteString)
                 , pathTableRefCount :: MyHashTable Word64 Word32
                 , as4 :: Bool
                 , updates :: IORef Int
                 , withdraws :: IORef Int
                }
instance Show Rib where show Rib{..} = if as4 then "AS4 Rib" else "AS2 Rib" 


print' = return
-- diagnotic to shorten the hash field only
-- myHash x = (hash64 x) .&. 0xfff
myHash = hash64

summary Rib{..} = do
  s1 <- toList prefixTable
  s2 <- toList pathTable
  uc <- readIORef updates
  wc <- readIORef withdraws
  return $ unlines [ "prefixTable: " ++ show (length s1)
                   , "pathTable: "  ++ show (length s2)
                   , "as4: " ++ show as4
                   , "updates: " ++ show uc
                   , "withdraws: " ++ show wc
                    ]

display Rib{..} = do
  s1 <- toList prefixTable
  s2 <- toList pathTable
  -- s3 <- toList pathTableRefCount
  return $ unlines [ "prefixTable", unlines $ map show s1
                   , "pathTable", unlines $ map show (map kv1 s2)
                   -- , "pathTableRefCount", unlines $ map show s3
                   , "as4: " ++ show as4 ]
  where kv1 (k,(v1,v2)) = (k,v1)

newRib :: IO Rib
newRib = do
    prefixTable <- (newSized 1000000)
    pathTable <- (newSized 100000)
    pathTableRefCount <- (newSized 100000)
    uc <- newIORef 0
    wc <- newIORef 0
    return $ Rib prefixTable pathTable pathTableRefCount False uc wc

newRib2 :: IO Rib
newRib2 = newRib
newRib4 :: IO Rib
newRib4 = do
    rib <- newRib
    return $ rib { as4 = True }

ribUpdateMany :: Rib -> ([PathAttribute],L.ByteString)-> [Prefix] -> IO()
ribUpdateMany Rib{..} (pathAttributes,bytes) prefixes = do
    unless (null prefixes)
           (modifyIORef' updates (1+))
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
    modifyIORef' updates (1+)
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
    unless (null prefixes)
           (modifyIORef' withdraws (1+))
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
    modifyIORef' withdraws (1+)
    oldPathHash <- mutate prefixTable prefix (\h -> (Nothing,fromJust h))
    newRefCount <- mutate pathTableRefCount oldPathHash f'
    when (0 == newRefCount)
         (do delete pathTable oldPathHash
             delete pathTableRefCount oldPathHash )
    where
        f' Nothing = error "hash is not present in ref count table"
        f' (Just 1) = (Nothing,0)
        f' (Just refCount) = (Just (refCount-1), refCount-1)
