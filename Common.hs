{-# LANGUAGE FlexibleInstances #-}
module Common(module Data.IP, module Common, module Hexdump) where
import Data.List(delete,sort,group,sortOn,foldl')
import Data.IP -- from package iproute
import Network.Socket (PortNumber) -- from package network
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import System.Time ( ClockTime (TOD) , getClockTime ) -- from package old-time
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict
import qualified Data.Map.Strict as DMS
import Data.Hashable
import Data.Foldable(toList)
import Numeric(showHex)

-- Debug stuff
--
import qualified Data.ByteString.Base16 as Base16 -- from package base16-bytestring
import Hexdump -- from package pretty-hex

hex :: Int -> String
hex x = Numeric.showHex x' "" where
  x' = (fromIntegral x) :: Word64
fromHex = fst . Base16.decode
fromHex' = L.fromStrict . fst . Base16.decode
toHex = C8.unpack . Base16.encode
toHex' = toHex . L.toStrict
simpleHex' = simpleHex . L.toStrict
prettyHex' = prettyHex . L.toStrict
-- 'local prelude'
--

groupBy_ :: (Eq k,Hashable k) => [(k, a)] -> [(k, [a])]
groupBy_ t = Data.HashMap.Strict.toList $ Data.HashMap.Strict.fromListWith (++) $ Prelude.map (\(a,b) -> (a,[b])) t

-- strictly, fromRight' (plain fromRight takes a default value to make it total)
-- and, not in GHC libs until at leats 8.2....

fromRight' :: Either a b -> b
fromRight' (Right b ) = b

type Fifo = Seq.Seq
emptyFifo = Seq.empty
nullFifo = Seq.null
fifo = Seq.fromList
enqueue s e = e Seq.<| s
dequeue s = (s',e) where (s' Seq.:> e) = Seq.viewr s
dequeueAll s = ( Seq.empty , Data.Foldable.toList s )
dequeueN n s = let (s1,s2) = Seq.splitAt n s in (s2, Data.Foldable.toList s1 )
peekAll s = Data.Foldable.toList s

-- application stuff
--

utcSecs = do
    (TOD sec psec) <- getClockTime
    return sec
-- a basic list inclusion test that you might have expected in Data.List
--
included [] _ = True
included ax [] | not (null ax) = False
included ax (b:bx) = included (delete b ax) bx

-- unsure which implementation of 'distribution' is better.....
-- distribution = map (\a -> (head a,length a)) . Data.List.group . Data.List.sort

distribution :: Ord a => [a] -> [(a,Int)]
distribution = sortOn ( (0 -) . snd) . DMS.toList . mapped
    where
    -- mapped :: (Ord a, Num b) => [a] -> DMS.Map a b
    mapped ix = foldl' f' DMS.empty ix
    -- f' :: (Ord a, Num b) => DMS.Map a b -> a -> DMS.Map a b
    f' m k = DMS.insertWith (+) k 1 m

distribution_ :: Integral a => Int -> [a] -> [(a,Int)]
distribution_ n a | length (distribution a) > n = a1 ++ [rollUp a2]
                  | otherwise = distribution a
                  where
    (a1,a2) = splitAt n (distribution a)
    rollUp [] = (0,0)
    -- rollUp ax = (fromIntegral $ length ax, sum (map snd a2))
    rollUp ax = (0, sum (map snd a2))

bgpPort = 179 :: PortNumber
ipV4_wildcard = toHostAddress ( read "0.0.0.0")
ipV4_localhost = toHostAddress ( read "127.0.0.1")

-- todo - make this a method of Binary by hiding the default method on import.....
putn :: Binary b => [b] -> Put
putn pfxs | null pfxs =  return ()
          | otherwise =  do put (head pfxs)
                            putn ( tail pfxs)
get16 :: Binary b => Get [b]
get16 = do
    bs <- getRemainingLazyByteString
    let lbs = fromIntegral $ L.length bs `div` 2
    getMany lbs

get32 :: Binary b => Get [b]
get32 = do
    bs <- getRemainingLazyByteString
    let lbs = fromIntegral $ L.length bs `div` 4
    getMany lbs

get64 :: Binary b => Get [b]
get64 = do
    bs <- getRemainingLazyByteString
    let lbs = fromIntegral $ L.length bs `div` 8
    getMany lbs

getn :: Binary b => Get [b]
getn = do
    empty <- isEmpty
    if empty
    then return []
    else do b <- get
            bs <- getn
            return (b:bs)

-- moved from RFC4721.hs

class Enum e => EnumWord8 e where
    decode8 :: Word8 -> e
    decode8 = toEnum . fromIntegral
    {-# INLINE decode8 #-}
    encode8 :: e -> Word8
    encode8 = fromIntegral . fromEnum
    {-# INLINE encode8 #-}


instance {-# OVERLAPPING #-} Binary (Word32,Word32,Word32) where
    put (w1,w2,w3) = put w1 <> put w2 <> put w3
    {-# INLINE put #-}
    get = do w1 <- get :: Get Word32
             w2 <- get :: Get Word32
             w3 <- get :: Get Word32
             return (w1,w2,w3)
    {-# INLINE get #-}

instance {-# OVERLAPPING #-} Binary [Word32] where
    put = putList'
        where putList' xs = putBuilder (Prim.primMapListFixed Prim.word32BE xs)

    get = do n <- get :: Get Word8
             getMany (fromIntegral n)

getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
     where
     go xs 0 = return $! reverse xs
     go xs i = do x <- get
                  x `seq` go (x:xs) (i-1)
-- {-# INLINE getMany #-}
