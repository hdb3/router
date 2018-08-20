module Report where
import Data.Word
import System.IO

import Common
import BGPparse
import GetBGPMsg
import Update
import PathAttributes
import Prefixes
import NewRib
import BGPData
import PrefixTable
import PrefixTableUtils
import AdjRIBOut

report (rib,adjrib) = do
        let fib = getFIB rib
            ribOut = getAdjRIBOut rib
            locRib = getRIB rib
            groomedAdjRib = groomAdjRIBList adjrib
        hPutStrLn stderr $ "got " ++ show (length fib) ++ " prefixes"
        hPutStrLn stderr $ "got " ++ show (length adjrib) ++ " updates for peer"
        hPutStrLn stderr $ "got " ++ show (length groomedAdjRib) ++ " groomed updates for peer"
        hPutStrLn stderr $ "got " ++ show (length ribOut) ++ " routes"
        hPutStrLn stderr $ "locRib size = " ++ show (length locRib)
        -- putStrLn $ showPrefixTable rib
        -- putStrLn "\n #############################\n"
        -- putStrLn $ showPrefixTableByRoute rib
