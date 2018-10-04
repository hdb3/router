{-# LANGUAGE RecordWildCards #-}
module BGPReader(readRib,bgpReader,readGroupedRib,pathReadRib) where
import System.IO
import System.Exit(die)
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get(runGet)

import Common
import Update
import BGPlib
import qualified Rib
import PrefixTableUtils(getRIB)
import qualified BGPData
import BogonFilter
import PathFilter

bgpReader :: FilePath -> IO [(BGPData.RouteData, BGPlib.Prefix)]
bgpReader path = do
    handle <- openBinaryFile path ReadMode
    stream <- L.hGetContents handle
    let bgpByteStrings = runGet getBGPByteStrings stream
        bgpMessages = map decodeBGPByteString bgpByteStrings
        updates = map getUpdate $ filter isUpdate bgpMessages
    rib <- Rib.newRib BGPData.dummyPeerData
    mapM_ (updateRib rib) updates
    rib' <- Rib.getLocRib rib
    return (getRIB rib')

updateRib rib parsedUpdate@ParsedUpdate{..} = do
                Rib.ribUpdater rib BGPData.dummyPeerData parsedUpdate

-- readRib: a convenience function for simple applications
-- the returned structure masks only derived or artificial data
-- - it contains the full parsed list of path attributes, associated prefixes (unpacked), and a route identifer which is unique in all cases
--   of path attribute sets, it is a hash of the original path attribute binary structure
--  However, it only contains the last version of the table, so earlier updates in the stream which were superceded are not returned


readRib :: IO [((Int, [PathAttribute]), BGPlib.Prefix)]
readRib = readUngroupedRib
readUngroupedRib = do rawRib <- readRib' 
                      return $ map normalise $ filter (bogonFilter . snd) rawRib

readGroupedRib :: IO [((Int, [PathAttribute]), [BGPlib.Prefix])]
readGroupedRib = do rawRib <- readRib' 
                    return $ map normalise $ applyBogonFilter $ groupBy_ rawRib
pathReadRib :: FilePath -> IO [((Int, [PathAttribute]), [BGPlib.Prefix])]
pathReadRib path = fmap ( applyPathFilter . map normalise . applyBogonFilter . groupBy_ ) ( bgpReader path)
--pathReadRib path = bgpReader path >>= map normalise . applyBogonFilter . groupBy_

readRib' = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then
        die "no filename specified"
    else do
        rib <- bgpReader (args !! 0)
        if n == 0 then
            return rib
        else
            return (take n rib)

normalise (routeData,a) = ((BGPData.routeId routeData , BGPData.pathAttributes routeData) , a)
