import System.IO

main :: IO()
main = do 
    eof <- isEOF
    if eof
        then return()
        else do
            l <- getLine
            putStrLn $ clockify l
            main

meridian :: Int -> String
meridian i
    | i >= 12   = "pm"
    | otherwise = "am"

minute :: Int -> String
minute i 
    | i == 0    = ""
    | i < 10    = "oh " ++ minutes!!i
    | i < 20    = minutes!!i
    | otherwise = tens!!(div i 10) ++ " " ++ minutes!!(mod i 10)

hour :: Int -> String
hour i 
    | i > 12    = hours!!(i-12)
    | otherwise = hours!!i
    
clockify :: String -> String
clockify s = do
    let parsed = split (==':') s
    let first = read(parsed!!0) :: Int
    let second = read(parsed!!1) :: Int
    "It's " ++ hour(first) ++ " " ++ minute(second) ++ " "++ meridian(first)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
                "" -> []
                s' -> w : split p s''
                    where (w, s'') = break p s'

-- lookup lists
hours :: [String]
hours   =   [ "twelve"  , "one"       , "two"      , "three"
            , "four"    ,"five"       , "six"      , "seven"
            , "eight"   , "nine"      , "ten"      , "eleven" 
            , "twelve"                                          ]

tens :: [String]
tens    =   [ ""        , ""          , "twenty"   , "thirty"   
            , "forty"   , "fifty"                               ]

minutes :: [String]
minutes =   [ ""        , "one"       , "two"      , "three"
            , "four"    , "five"      , "six"      , "seven"
            , "eight"   , "nine"      , "ten"      , "eleven"
            , "twelve"  , "thirteen"  , "fourteen" , "fifteen"
            , "sixteen" , "seventeen" , "eighteen" , "nineteen" ]