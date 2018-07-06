{-# LANGUAGE RecordWildCards #-}
module Main where
import Open
import Capabilities

main = do 
    let sm = makeOpenStateMachine Offer {..} Required {..} where
        myAS = 1234
        holdTime = 40
        bgpID = 65520
        requiredAS = Just 4321
        requiredHoldTime = Just 20
        requiredBgpID = Nothing
        requiredCapabilities = [CapAS4 65520,  CapGracefulRestart False 0]
        optionalCapabilities = [ CapAS4 65520,  CapGracefulRestart False 0]
    putStr "initialState: "
    print sm

    let receivedOffer = Offer {..} where
        myAS = 4321
        holdTime = 30
        bgpID = 65521
        optionalCapabilities = [ CapGracefulRestart False 0]
    putStrLn ""
    putStr "receivedOffer: "
    print receivedOffer

    let sm' = updateOpenStateMachine sm receivedOffer
    putStrLn ""
    putStr "updatedState: "
    print sm'

    putStrLn ""
    putStr "status: "
    print $ getStatus sm'

    putStrLn ""
    putStr "response: "
    print $ getResponse sm'

    
