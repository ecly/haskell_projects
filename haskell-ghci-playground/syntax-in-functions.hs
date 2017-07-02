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