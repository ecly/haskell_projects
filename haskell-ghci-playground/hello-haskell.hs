-- combining functions
doubleMe x = x 
doubleUs x y = doubleMe y + doubleMe x

fiveFives = take 5 $ cycle [5]
alsoFiveFives = take 5 $ repeat 5
fiveFivesAgain = replicate 5 5 

-- composition
tenOddNumbers = [x*2-1 | x <- [1..10]]

-- predicate/filtering
alsoTenOddNumbers = [x | x <- [1..20], mod x 2 == 1]

boomBangs xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]
boomBangTest = boomBangs [1..20]

-- multiple predicates
fuckTensAndOdds = [x | x <- [1..20], even x, not $ elem x [0,10..100]]

-- multiple lists
coolBeans = [x + y | x <- [1..4], y <- [1..4], odd x, even y]

-- length implementation
length' xs = sum [1 | _ <- xs]

-- strings are lists of chars
plusOneEncoded = [succ c | c <- "ABC"]

-- working on multiple lists
xss = [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15]]
odds = [[x | x <- xs, odd x] | xs <- xss]

-- zipping tuples
a = [x | x <- [1..10], odd x]
b = [x | x <- [1..10], even x]
evenAndOdds = zip a b

-- zipping with infinite lists
infiniteInts = [1..]
numberedFruits = zip infiniteInts ["mango", "banana", "pineapple"]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]   
rightTriangles = [(a,b,c) | (a,b,c) <- triangles, a^2 + b^2 == c^2]
rightTriangles' = [(a,b,c) | (a,b,c) <- triangles, a^2 + b^2 == c^2, a+b+c == 24]