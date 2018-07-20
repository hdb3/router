{-# LANGUAGE RecordWildCards #-}
module Rib where
import FarmHash(hash64)
import Data.HashTable.IO
import Control.Monad(when)
import Data.Word
import Data.Maybe(fromJust)
import qualified Data.ByteString as B

import Prefixes
import PathAttributes

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


display Rib{..} = do
  s1 <- toList prefixTable
  s2 <- toList pathTable
  s3 <- toList pathTableRefCount
  return $ unlines [ "prefixTable", show s1
                   , "pathTable", show s2
                   , "pathTableRefCount", show s3
                   , "as4", show $ as4 ]

newRib :: IO Rib
newRib = do
    prefixTable <- (newSized 1000000) -- :: LinearHashTable Prefix Word64
    pathTable <- (newSized 100000)  -- :: LinearHashTable Word64 ([PathAttribute],B.ByteString)
    pathTableRefCount <- (newSized 100000)  -- :: LinearHashTable Word64 Word32
    return $ Rib prefixTable pathTable pathTableRefCount False

newRib2 :: IO Rib
newRib2 = newRib
newRib4 :: IO Rib
newRib4 = do
    rib <- newRib
    return $ rib { as4 = True }

{-
ribInsert :: Rib -> Prefix -> ([PathAttribute],ByteString) -> IO()
ribInsert Rib{..} prefix (pathAttributes,bytes) = do
    let hash = hash64 bytes
    insert prefixTable prefix hash
    insert pathTable hash (pathAttributes,bytes)
-}
-- insert is useful except that unwanted paths are never removed which will cause trouble over time
-- however the alternative is to maintain use counts......
-- logic: if the prefix 
--

ribUpdate :: Rib -> Prefix -> ([PathAttribute],B.ByteString) -> IO()
ribUpdate Rib{..} prefix (pathAttributes,bytes) = do
    let hash = hash64 bytes
    oldHashM <- mutate prefixTable prefix (\v -> (Just hash, v))
    -- updatePathTable (hash,pathAttributes,bytes)
    putStrLn ""
    newRefCount <- mutate pathTableRefCount hash f' 
    putStrLn ""
    when (1 == newRefCount)
        (insert pathTable hash (pathAttributes,bytes))
    where
        f' Nothing = (Just 1, 1)
        f' (Just refCount) = (Just (refCount+1), refCount+1)

ribWithdraw :: Rib -> Prefix -> IO()
ribWithdraw Rib{..} prefix = do
    oldPathHash <- mutate prefixTable prefix (\h -> (Nothing,fromJust h))
    newRefCount <- mutate pathTableRefCount oldPathHash f'
    when (0 == newRefCount)
        (delete pathTable oldPathHash)
    where
        f' Nothing = error "prefix is not present in ref count table"
        f' (Just 1) = (Nothing,0)
        f' (Just refCount) = (Just (refCount-1), refCount-1)

{-
-- the mutate user function for the prefix table gets the old path hash wrapped in maybe
-- and must return a tuple whose 1st value is the new value
-- the second element in the tuple is returned as the result of the mutate
--
-- in our case we always want to return the same new value, which is the new hash
-- however we want to update the path table using the old value, if there is one
-- we can use the return value for this - a Maybe over the old hash
-- here is the required function 'f'
   f Nothing = (Just hash, Nothing)
   f (Just oldHash) = (Just hash, Just oldHash)
-- from inspection we can see that this is really just:
   f m = (Just hash, m)
-- 
-- The operation on the pathTable is a simple add-update of the ref count for the new hash - 
-- if there is no old hash stored the use case goes to 1, other wise it goes to N+1
-- (though a check would not be amiss!!!!)
--  If there is an old hash then there is also a del-update required
--
--  addUpdate: if the hash is not found in the usecount table then set it to one
--             otherwise increment it.  If it was not found then it also needs to be added to the
--             main table - i.e. 
    f' Nothing = (Just 1, 1)
    f' (Just refCount) = (Just (refCount+1), refCount+1)
    addUpdate hash = do

-- delUpdate: a similar mutate on the ref count table, however if the hash is not found that would be a logic error...
-- the user function should 
    f'' Nothing = error "prefix is not present in ref count table"
    f'' (Just 1) = (Nothing,0)
    f'' (Just refCount) = (Just (refCount-1), refCount-1)
-}
