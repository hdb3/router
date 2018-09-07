module Main where
import qualified Data.Sequence
import Common

printFifo r = showFifo r >>= putStrLn
expect :: IO String -> String -> IO ()
expect  a b = do
    r <- a
    if b == r then
        putStrLn "OK"
    else
        putStrLn $ "Fail - " ++ r ++ " <> " ++ b

show' x = return (show x)
main = do
    fifo1 <- fifo [1,2,3]
    -- printFifo fifo1
    expect (showFifo fifo1) "fromList [1,2,3]"
    n <- peekAll fifo1
    -- print n
    expect (show' n) "[1,2,3]"
    enqueue fifo1 4
    -- printFifo fifo1
    expect (showFifo fifo1) "fromList [4,1,2,3]"
    k <- dequeue fifo1
    -- print k
    expect (show' k) "3"
    -- printFifo fifo1
    expect (showFifo fifo1) "fromList [4,1,2]"
    p1 <- nullFifo fifo1
    expect (show' p1) "False"
    -- print p1
    kx <- dequeueAll fifo1
    -- print kx
    expect (show' kx) "[4,1,2]"
    -- printFifo fifo1
    expect (showFifo fifo1) "fromList []"
    p2 <- nullFifo fifo1
    expect (show' p2) "True"
    -- print p2

{-
fromList [1,2,3]
[1,2,3]
fromList [4,1,2,3]
3
fromList [4,1,2]
False
[4,1,2]
fromList []
True
-}
