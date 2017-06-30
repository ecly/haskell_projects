main = do
    line <- getLine
    print $ split (==':') line

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
                "" -> []
                s' -> w : split p s''
                    where (w, s'') = break p s'

times = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve"]

