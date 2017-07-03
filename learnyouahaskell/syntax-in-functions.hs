{- Mostly just duplicates as seen at http://learnyouahaskell.com/ -}
-- read from top down -> order matters
lucky 7 = "You won" 
lucky x = "Nice try idiot"

lucky' x 
    | x == 7 = "You won"
    | otherwise = "Nice try idiot"

-- recursion
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- matching tuples
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- pattern matching in list comprehension
xs = [(1,1), (2,2), (3,3)]
xs' = [a+b | (a,b) <- xs]

-- pattern matching lists generally
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

-- binding entire list
firstLetterOfWord all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

-- guards
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"   

-- inline guards... don't use these
max' a b | a > b = a | otherwise = b  

-- 'infix' function definition
a `eq` b 
    | a == b = True
    | otherwise = False

-- where -> eg. avoid repetition
bmiTell' weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  

-- multiple where expressions
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

-- let bindings => let <bindings> in <expression>
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- let bindings can be used almost anywhere unlike where eg.
res' = 4 * (let a = 9 in a + 1) + 2  
res'' = [let square x = x * x in (square 5, square 3, square 2)]  

-- multiple inline let binds -> separate with semicolon
res1 = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  

-- list comprehenseion with let binds
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  

-- case expressions
head' :: [a] -> a  
head'' xs = case xs of [] -> error "No head for empty lists!"  
                       (x:_) -> x  

{- case expression of pattern -> result  
                    pattern -> result  
                    pattern -> result  
                    ...  
-}

-- where / case comparison example
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  