-- main :: IO ()
-- main = putStrLn "hello, world"

-- do syntax lets us glue multiple I/O actions into one
-- The action that this main gives us is IO ()
-- This is due to it being the type of the last I/O action
-- inside the do block.
main = do
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
