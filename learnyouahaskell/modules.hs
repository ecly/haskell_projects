import Data.List hiding (sort)
-- hiding allows to exclude certain functions
import Data.List (sort)
-- we can also import only specific functions from a module
import qualified Data.Array 
-- qualified import means we have to call functions like Data.Array.sort

-- total list of modules in standard library:
-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/

-- nub removes duplications from lists
numberOfUniques :: (Eq a) => [a] -> Int
numberOfUniques = length . nub

-- extra functions from Data.List

-- folds have stricter implementatons denoted by '
nonLazyFolding = foldl1' (+) [1,1,1]

-- flattens a list of lists into a list
flattenedList = concat ["h", "e", "y"]

-- returns true -> works for or as well
allNumbersPositive = and $ map (>0) [1,2,3]

-- any and all do this perhaps more neatly (like linq)
elem4 = any (==4) [1,2,3,4]
allNumbersNegative = all (<0) [1,2,3,4]


-- iterate, iterates an action infinite
powersOfTwo = take 10 $ iterate (*2) 1  
-- > [1,2,4,8,16,32,64,128,256,512]  

-- takeWhile is still cool (note, dropWhile exists as well)
coolestBeans = takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
-- [6,5,4]  


-- combining all these cool functions
-- sum of all third powers that are under 10.000
sumSum = sum $ takeWhile (<10000) $ map (^3) [1..]  
-- = 53361


-- lambda currying and dropWhile
-- when does fst go beyond 1000 for the first time
stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
answer = head (dropWhile (\(val,y,m,d) -> val < 1000) stock)