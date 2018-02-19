import Data.Char
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
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

-- The when function
