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

ribUpdateMany :: Rib -> ([PathAttribute],B.ByteString)-> [Prefix] -> IO()
ribUpdateMany rib attrs prefixes = Prelude.mapM_ (ribUpdate rib attrs) prefixes

ribUpdate :: Rib -> ([PathAttribute],B.ByteString) -> Prefix -> IO()
ribUpdate Rib{..} (pathAttributes,bytes) prefix = do
    let hash = hash64 bytes
    oldHashM <- mutate prefixTable prefix (\v -> (Just hash, v))
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
