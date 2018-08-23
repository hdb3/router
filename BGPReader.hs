{-# LANGUAGE RecordWildCards #-}
module BGPReader where
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get(runGet)

import Update
import BGPparse
import NewRib
import GetBGPMsg
import BGPData(defaultPeerData)
import PrefixTable(PrefixTable)

bgpReader :: FilePath -> IO PrefixTable
bgpReader path = do
    handle <- openBinaryFile path ReadMode
    stream <- L.hGetContents handle
    let msgs = runGet getBGPByteStrings stream
        parsedMsgs = map decodeBGPByteString msgs
        updates = map getUpdateP $ filter isUpdate $ getUpdatesFrom parsedMsgs
        -- updates = map getUpdateP $ getUpdatesFrom parsedMsgs
    -- hPutStrLn stderr $ "read " ++ show (length updates) ++ " updates"
    rib <- newRib
    mapM_ (updateRib defaultPeerData rib) updates
    getRib rib

updateRib peer rib BGPUpdateP{..} = do
                ribUpdateMany rib peer attributesP hashP nlriP
                ribWithdrawMany rib peer withdrawnP

-- TODO rewrite this as a simple filter???
getUpdatesFrom :: [BGPMessage] -> [BGPMessage]
getUpdatesFrom = filter isUpdate
getUpdatesFrom' :: [BGPMessage] -> [BGPMessage]
getUpdatesFrom' = foldr keepOnlyUpdates [] where
                          keepOnlyUpdates a@BGPUpdate{} ax = a:ax
                          keepOnlyUpdates _ ax = ax
