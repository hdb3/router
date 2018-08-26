{-# LANGUAGE RecordWildCards #-}
module BGPReader(readRib,bgpReader,Rib,readGroupedRib) where
import System.IO
import System.Exit(die)
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get(runGet)
import Control.Monad(liftM)

import Common
import Update
import BGPparse
import qualified NewRib as NR
import GetBGPMsg
import BGPData(defaultPeerData)
import PrefixTable(PrefixTable)
import PrefixTableUtils(getRIB,getAdjRIBOut)
import qualified Prefixes
import qualified PathAttributes
import qualified BGPData
import BogonFilter

type Rib = [((Int,[PathAttributes.PathAttribute]), [Prefixes.Prefix])]
-- bgpReader :: FilePath -> IO Rib
bgpReader path = do
    handle <- openBinaryFile path ReadMode
    stream <- L.hGetContents handle
    let msgs = runGet getBGPByteStrings stream
        parsedMsgs = map decodeBGPByteString msgs
        updates = map getUpdateP $ filter isUpdate parsedMsgs
    rib <- NR.newRib
    mapM_ (updateRib defaultPeerData rib) updates
    rib' <- NR.getRib rib
    let groupedRib = map normalise $ applyBogonFilter $ groupBy_ (getRIB rib')
        ungroupedRib = map normalise $ filter (bogonFilter . snd) (getRIB rib')
    return (getRIB rib')

updateRib peer rib BGPUpdateP{..} = do
                NR.ribUpdateMany rib peer attributesP hashP nlriP
                NR.ribWithdrawMany rib peer withdrawnP

-- readRib: a convenience function for simple applications
-- the returned structure masks only derived or artificial data
-- - it contains the full parsed list of path attributes, associated prefixes (unpacked), and a route identifer which is unique in all cases
--   of path attribute sets, it is a hash of the original path attribute binary structure
--  However, it only contains the last version of the table, so earlier updates in the stream which were superceded are not returned

readUngroupedRib = do rawRib <- readRib' 
                      return $ map normalise $ filter (bogonFilter . snd) rawRib

readGroupedRib = do rawRib <- readRib' 
                    return $ map normalise $ applyBogonFilter $ groupBy_ rawRib

readRib = readUngroupedRib

readRib' = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then do
        die "no filename specified"
    else do
        rib <- bgpReader (args !! 0)
        if n == 0 then
            return rib
        else
            return (take n rib)

normalise (routeData,a) = ((BGPData.routeId routeData , BGPData.pathAttributes routeData) , a)
