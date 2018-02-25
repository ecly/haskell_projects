import System.IO
import System.Environment
import Data.List
import Data.Char
import Control.Monad
-- main :: IO ()
-- main = putStrLn "hello, world"

-- do syntax lets us glue multiple I/O actions into one
-- The action that this main gives us is IO ()
-- This is due to it being the type of the last I/O action
-- inside the do block.
main0 = do
    print "What is your name?"
    name <- getLine
    print $ "Hey " ++ name ++ ", you are OK!"

-- :t getLine
-- getLine :: IO String
--
-- Returns an IO monads.
-- name <- getLine -- bind the contained value of the IO Monad to name
--
-- The only way to snatch out data from inside IO String is to be inside another
-- I/O action, hence the do syntax.
--
--
-- We can call pure functions from within impure functions, giving them parameters
-- from eg. name. The pure function stays pure despite getting the name from impure context.
--
main1 = do
    -- binds foo to ()
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    -- the last call may never bind
    putStrLn ("Hey " ++ name ++ ", you rock!")


-- name = getLine, is merely making an alternative name for the function getLine.

-- We use let bindings to bind variables expressions to names in do blocks
main2 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- Use <- for bindings monads in do blocks, and use let for binding pure expressions
--
--
-- For main loops, we write them recursively.
main3 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- without function composition (yuck)
reverseWords1 :: String -> String
reverseWords1 st = unwords (map reverse (words st))

-- If we don't want to compile and run, we can use the runhaskell command like so:
-- runhaskell input-and-output.hs
--
-- return in haskell, in the context of and I/O action, it returns a pure value encapsulated
-- in an I/O monad. return () -> IO (), return "haha" -> IO String
--
-- The reason we do return () is because we need or program return an IO action even in the else
-- case. For that reason we just make a bogus I/O action.
--
-- Return doesn't exit a function. Eg. below is valid and will carry out all the way to the last line.
main4 = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

-- To show this, and the encapsulation of return's arguments,
-- below code does exactly what one might think
main5 = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

-- If we don't use a return, we just return our last result value of the last actoin
-- in the do block.
--
--
-- Extra useful I/O functions
main6 = do
    putStr "Hey" -- no newline here
    putChar 'm'
    putChar '8'
    putStrLn ""

-- 'print' is like putStrLn except it calls show on the parameter prior to printing.
-- As such it is defined like this:
--
-- print1 = putStrLn . show

-- getChar, like getLine except it reads a single character thus returning IO Char
-- - Interestingly here, giving input "hello sir" return, will print "hello".
main7 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

-- The when function
-- When is a function that takes a boolean and an I/O action.
-- If the boolean is true, it returns the supplied I/O action, otherwise it returns ()
main8 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

-- We can sequence together I/O actions using the sequence function
main9 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- Mapping over something and sequencing the result is a common action,
-- hence there is a special mapM for exactly this:
print123 = mapM print [1,2,3]

-- Above will additionally return the result of the I/O actions, meaning [(), (), ()]
-- If we don't want the resulting list returned, we can use mapM_

-- Instead of making our main funciton recursive, we can alternatively in some cases
-- use the forever function as below:
main10 = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

-- forM is mapM with parameters reversed which is more readable now and then

-- Files and streams
-- getContents :: IO String
-- Lazily reads everything from STDIN until it encounters EOF character.
--
-- See scripts [capslocker.hs] and [shortLinesOnly.hs] for examples...
--
-- Files:
--
main11 = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- openFile :: FilePath -> IOMode -> IO Handle
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- hGetContents is simply getContents on handles, naturally lazily
-- NOTE: we have to close the file handle ourselves with hClose
--
-- A more concise way of doing the above is using:
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- which is similar to open with in Python
main12 = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

-- there are h-versions of most of the I/O functions we've seen
-- hGetLine, hPutStr, hPutStrLn, hGetChar
-- They take an additional handle as parameter and operate on that handle's file instead
--
-- readFile will close files for us automatically and is quite concise:
main13 = do
    contents <- readFile "girlfriend.txt"
    putStr contents

-- read a file, capitalize the contents and make a new file with the capitalized contents
main14 = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)

-- appending to existing files
main15 = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

-- reading by chunks as alternative to default line by life buffering
-- the Maybe Int is the chunk size - if nothing is given,
-- the OS will determine the buffer size
main16 = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)

-- See ../scripts/todoManager.hs for a 'full' example on handling an existing file.
--
-- Command line arguments:
-- Utlize useful funcitons from System.Environment
-- for handling command line arguments
main17 = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM_ putStrLn args
   putStrLn "The program name is:"
   putStrLn progName
