alphabet :: Char -> [Char]
alphabet c 
    | c == 'a' = "bc"
    | c == 'b' = "a"
    | c == 'c' = "aaa"

collatz :: [Char] -> IO()
collatz s = 
    do
        putStrLn s
        if s == "a" then return()
        else
            collatz $ drop 2 s ++ alphabet (head s)
        
