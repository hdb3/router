{-# LANGUAGE RecordWildCards #-}
module Main where
import Open
import Capabilities
import RFC4271
import BGPparse

main = mapM_ runTest [test1,test2,test3]

test1 = ("test1",
         BGPOpen 1234 40 65520 [ CapAS4 65520,  CapGracefulRestart False 0],
         -- Offer { myAS = 1234, offeredHoldTime = 40, offeredBGPid = 65520, optionalCapabilities = [ CapAS4 65520,  CapGracefulRestart False 0] },
         BGPOpen 4321 20 0 [CapGracefulRestart False 0],
         BGPOpen 4321 30 65521 [ CapGracefulRestart False 0],
         BGPKeepalive)

test2 = ("test2", loc',req',rec',res') where
        (_, loc,req,rec,res) = test1
        loc' = loc
        req' = req { caps = [CapGracefulRestart False 0, CapAS4 65521]}
        rec' = rec 
        res' = BGPNotify NotificationOPENMessageError UnsupportedOptionalParameter [CapAS4 65521]

test3 = ("test3", loc',req',rec',res') where
        (_, loc,req,rec,res) = test1
        loc' = loc
        req' = req { holdTime = 40 }
        rec' = rec 
        res' = BGPNotify NotificationOPENMessageError UnacceptableHoldTime []


runTest (desc,loc,req,rec,expect) = do 
    let sm = makeOpenStateMachine loc req
    let sm' = updateOpenStateMachine sm rec
    let resp =  getResponse sm'


    if expect == resp
        then putStrLn $ desc ++ ": success"
        else do
            putStrLn $ desc ++ ": ***fail***"
            putStrLn "expected: "
            print expect
            putStrLn "got: "
            print resp
            putStr "initialState: "
            print sm

            putStrLn ""
            putStr "receivedOffer: "
            print rec

            putStrLn ""
            putStr "updatedState: "
            print sm'

            putStrLn ""
            putStr "holdTime: "
            print $ getNegotiatedHoldTime sm'
