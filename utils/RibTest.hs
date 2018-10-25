module Main where
import RibDef
import RIBData

main = do
    putStrLn "RibTest"
    let rib = mkRib compare :: MapRib
    -- let rib = mkRib (compare :: ((Peer,Route) -> (Peer,Route) -> Ordering))
    print rib
