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

-- find the sum of all odd squares that are smaller than 10,000

-- using takeWhile
sumOfOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- using list comprehension
sumOfOddSquares' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  

-- applying partial multiplication in nested list comprehension
multiplyBy = map (*) [0..]
nestedListComprehensions = take 5 [[x y | y <- [0,1] ] | x <- multiplyBy]


-- using lambdas 
listsLongerThan1 = filter (\xs -> length xs > 1) [[1], [1,2]]


-- example of silly lambda situation
-- why would we use lambda for something that partial functions can do
badLambda = map (\x -> x + 3) [1,6,3,2]

thisIsSmarter = map (+3) [1,6,3,2]


-- pattern matching in lambda
-- NOTE -> runtime error if pattern matching in lambda fails
patternMatchingLambda = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 


-- lambda and currying -> below 2 functions are equivalent
addThree x y z = x + y + z  
addThree' = \x -> \y -> \z -> x + y + z


-- folding in haskell
-- foldl folds from the left
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

-- writing foldl sum more succintly using curryings
-- note xs param can be omitted as sum'' wil return a function that takes a list
sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0  

-- right fold!
-- accumulator on the right and input list on the left (in contrast to foldl)
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  

-- right folds are usually used when building up new lists so we avoid appending