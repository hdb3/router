{-# LANGUAGE RecordWildCards #-}
module Main where
import Open
import Capabilities
import RFC4271

main = mapM_ runTest [test1,test2,test3]

test1 = ("test1",
         Offer { myAS = 1234, holdTime = 40, bgpID = 65520, optionalCapabilities = [ CapAS4 65520,  CapGracefulRestart False 0] },
         Required { requiredAS = Just 4321, requiredHoldTime = Just 20, requiredBgpID = Nothing, requiredCapabilities = [CapGracefulRestart False 0]},
         Offer { myAS = 4321, holdTime = 30, bgpID = 65521, optionalCapabilities = [ CapGracefulRestart False 0] },
         Nothing)

test2 = ("test2", loc',req',rec',res') where
        (_, loc,req,rec,res) = test1
        loc' = loc
        req' = req { requiredCapabilities = [CapGracefulRestart False 0, CapAS4 65521]}
        rec' = rec 
        res' = Just (NotificationOPENMessageError,UnsupportedOptionalParameter,Just (CapAS4 65521))

test3 = ("test3", loc',req',rec',res') where
        (_, loc,req,rec,res) = test1
        loc' = loc
        req' = req { requiredHoldTime = Just 40 }
        rec' = rec 
        res' = Just (NotificationOPENMessageError,UnacceptableHoldTime,Nothing)


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
            putStr "status: "
            print $ getStatus sm'
