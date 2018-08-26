{-# LANGUAGE RecordWildCards #-}
module BGPReader(readRib,bgpReader,Rib) where
import System.IO
import System.Exit(die)
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get(runGet)

import Update
import BGPparse
import qualified NewRib as NR
import GetBGPMsg
import BGPData(defaultPeerData)
import PrefixTable(PrefixTable)
import PrefixTableUtils(getAdjRIBOut)
import qualified Prefixes
import qualified PathAttributes
import qualified BGPData
import BogonFilter

type Rib = [((Int,[PathAttributes.PathAttribute]), [Prefixes.Prefix])]
bgpReader :: FilePath -> IO Rib
bgpReader path = do
    handle <- openBinaryFile path ReadMode
    stream <- L.hGetContents handle
    let msgs = runGet getBGPByteStrings stream
        parsedMsgs = map decodeBGPByteString msgs
        updates = map getUpdateP $ filter isUpdate parsedMsgs
    rib <- NR.newRib
    mapM_ (updateRib defaultPeerData rib) updates
    rib' <- NR.getRib rib
    return $ applyBogonFilter $ dump rib'

updateRib peer rib BGPUpdateP{..} = do
                NR.ribUpdateMany rib peer attributesP hashP nlriP
                NR.ribWithdrawMany rib peer withdrawnP

-- readRib: a convenience function for simple applications
readRib = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then do
        die "no filename specified"
    else do
        rib <- bgpReader (args !! 0)
        if n == 0 then
            return rib
        else
            return ( take n rib)

dump :: PrefixTable -> Rib
dump prefixTable = let f (routeData,ipfxs) = ((BGPData.routeId routeData , BGPData.pathAttributes routeData) , Prefixes.toPrefixes ipfxs) in map f (getAdjRIBOut prefixTable)
-- dump prefixTable = let f (routeData,ipfxs) = (BGPData.pathAttributes routeData,Prefixes.toPrefixes ipfxs) in map f (getAdjRIBOut prefixTable)
