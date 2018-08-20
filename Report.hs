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

report rib = do
        let fib = getFIB rib
            ribOut = getAdjRIBOut rib
            locRib = getRIB rib
        hPutStrLn stderr $ "got " ++ show (length fib) ++ " prefixes"
        hPutStrLn stderr $ "got " ++ show (length ribOut) ++ " routes"
        hPutStrLn stderr $ "locRib size = " ++ show (length locRib)
