module Main where
import TypeCodePathAttributes
import Control.Exception(assert)

main = do
    mapM_ testCode allPathAttributeTypeCodes where
    testCode c = assert ( c == (toEnum . fromEnum) c ) (putStrLn $ show c ++ " - OK" )
