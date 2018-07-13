module Main where
import PathAttributes
import Control.Exception(assert)

main = do
    mapM_ testCode allPathAttributeTypeCodes where
    testCode c = assert ( c == (toEnum . fromEnum) c ) (putStrLn $ show c ++ " - OK" )
