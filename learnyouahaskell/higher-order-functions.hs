{-  everything is a function that takes one param
    as such, below works

    ghci> let multTwoWithNine = multThree 9  
    ghci> multTwoWithNine 2 3  
    54  
    ghci> let multWithEighteen = multTwoWithNine 2  
    ghci> multWithEighteen 10  
    180  
-}

-- example of identical compare functions
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  

compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100  

-- applying infix functions partially
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  
-- ghci> divideByTen 20 -> 2.0

-- special case is negative numbers and thereby minus operator
-- (-4) is negative 4 and not partial application of minus operator
-- therefore the subtraction equivalent is as seen below
subtractByFour :: Integer -> Integer
subtractByFour = (subtract 4)

-- functions as input

-- functions need parenthesis in type declaration
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

-- eg. ghci> applyTwice (/10) 100 -> 1


-- zip with implementation 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

-- eg.
-- zipWith' (\x y -> (x,y)) [1,2,3] [3,2,1]
-- [(1,3),(2,2),(3,1)]

filterExample = filter (>3) [1,5,3,2,1,6,4,3,2,1]  

-- quicksort with filtering
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

-- find the largest number under 100,000 that's divisible by 3829. 
largestDivisible :: Integer
largestDivisible = head [x | x <- [100000,99999..], x `mod` 3829 == 0 ]

largestDivisible' :: (Integral a) => a  
largestDivisible' = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  