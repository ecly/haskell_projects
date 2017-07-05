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
-- also, right folds work on infinite lists but left folds do not
-- with a right fold you will eventually reach the beginning of the list


-- foldr1 and foldl1 assume the acc based on first or last value of given list

-- implementation of standard library functions using fold
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
    
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
    
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
    
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
    
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
    
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  


--scanl and scanr is like fold except all intermediate values are stored in the output aswell
-- ghci> scanr (+) 0 [3,5,2,1]  
-- ghci> [11,8,3,1,0]  


-- using $ for precedence
-- square root of (3+4+9)
thatsNotRight = sqrt 3 + 4 + 9
whatWeWant = sqrt (3 + 4 + 9)
coolWayToGetIt = sqrt $ 3 + 4 + 9


-- function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
-- f . g = \x -> f (g x)  

-- making all values negative 
sillyWayWithLambda = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
coolWayWithFunctionComposition = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

-- function composition is right associative (if that wasn't clear already)
-- meaning f (g (z x)) == (f . g . z) x

-- function composition with more than one parameter???
letsChangeThis = sum (replicate 5 (max 6.7 8.9))
functionComposition = (sum . replicate 5 . max 6.7) 8.9
functionComposition' = sum . replicate 5 . max 6.7 $ 8.9

letsChangeThis' = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
composed = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]


-- point free style / implicit params / currying
-- below are identical
fn x = ceiling (negate (tan (cos (max 50 x))))  
fn' = ceiling . negate . tan . cos . max 50  

-- when function composition gets out of hand!
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))   
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  

-- now in a readable fashion!
oddSquareSumReadable =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  