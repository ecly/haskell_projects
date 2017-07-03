{- Mostly just duplicates as seen at http://learnyouahaskell.com/ -}
-- simple recursive fib using guards
fib n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

-- maximum function defined recursively 
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

-- maximum function rewritten using max
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum''[x] = x  
maximum'' (x:xs) = max x (maximum' xs)  


-- replicate written recursively
replicate' n x
    | n == 0 = []
    | otherwise = x:replicate' x (n-1)

-- take recursively
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x:take' (n-1) xs

-- reverse recursively
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat recursively 
-- loops forever but called with eg. take due to lazy evaluation works fine
repeat' :: a -> [a]  
repeat' x = x:repeat' x  

-- zip recursively
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  

-- elem recursively
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem a xs

-- quicksort recursively
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  