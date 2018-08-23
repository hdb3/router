{-# LANGUAGE RecordWildCards #-}
module BGPReader where
import System.IO
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

type Rib = [([PathAttributes.PathAttribute], [Prefixes.Prefix])]
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
    return $ dump rib'

updateRib peer rib BGPUpdateP{..} = do
                NR.ribUpdateMany rib peer attributesP hashP nlriP
                NR.ribWithdrawMany rib peer withdrawnP

dump :: PrefixTable -> Rib
dump prefixTable = let f (routeData,ipfxs) = (BGPData.pathAttributes routeData,Prefixes.toPrefixes ipfxs) in map f (getAdjRIBOut prefixTable)
