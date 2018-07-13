module Main where
-- import System.Environment
-- import Network.Socket
-- import Common
-- import BgpFSM
-- import BGPparse
-- import Capabilities
import Args

main = do
    config <- getConfig
    -- print config
    either (\s -> putStrLn $ "failed: <" ++ s ++ ">")
           (\conf -> putStrLn $ "success " ++ show conf)
           config
