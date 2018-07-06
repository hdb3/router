{-# LANGUAGE RecordWildCards #-}
module Main where
import Open
import Capabilities

test1 = (Offer { myAS = 1234, holdTime = 40, bgpID = 65520, optionalCapabilities = [ CapAS4 65520,  CapGracefulRestart False 0] },
         Required { requiredAS = Just 4321, requiredHoldTime = Just 20, requiredBgpID = Nothing, requiredCapabilities = [CapAS4 65520,  CapGracefulRestart False 0]},
         Offer { myAS = 4321, holdTime = 30, bgpID = 65521, optionalCapabilities = [ CapGracefulRestart False 0] })

runTest (loc,req,rec) = do 
    let sm = makeOpenStateMachine loc req
    putStr "initialState: "
    print sm

    putStrLn ""
    putStr "receivedOffer: "
    print rec

    let sm' = updateOpenStateMachine sm rec
    putStrLn ""
    putStr "updatedState: "
    print sm'

    putStrLn ""
    putStr "status: "
    print $ getStatus sm'

    putStrLn ""
    putStr "response: "
    print $ getResponse sm'

main = do 
    runTest test1
