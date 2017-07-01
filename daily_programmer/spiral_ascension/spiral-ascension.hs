import System.IO
import Data.Matrix

main :: IO()
main = do 
    eof <- isEOF
    if eof
        then return()
        else do
            l <- getLine
            spiralize $ read l
            main

spiralize :: IO()
spiralize i = return()
