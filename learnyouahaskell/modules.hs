import Data.List hiding (sort)
-- hiding allows to exclude certain functions
import Data.List (sort)
-- we can also import only specific functions from a module
import qualified Data.Array 
-- qualified import means we have to call functions like Data.Array.sort

import qualified Data.Map as Map -- abbreviation
import qualified Data.Set as Set -- abbreviation

import Data.Char

-- hackero lint error cause not using stack, but compiles just fine
import Geometry

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


-- search for sublists
returnsTrue = "cat" `isInfixOf` "im a cat burglar"  
returnsFalse = "Cat" `isInfixOf` "im a cat burglar"  

-- similiarly exists isPrefixOf and isSuffixOf
-- behaving like StartsWith and EndsWith


-- partions splits a list into a tuple of lists.
-- one containing the elements that satisfy the predicate and one that does not
splitThisListBaby = partition (>3) [1,3,5,6,3,2,1,0,3,7]  
-- result: ([5,6,7],[1,3,3,2,1,0,3])  

-- find returns a Maybe with the first occurence of elemenet satisfying predicate
fourQuestionMark = find (>4) [1,2,3,4,5,6]  
-- Just 5


-- operations on characters
_ = all isAlphaNum "dankmemes69" -- true
_ = all isAlpha "dankmemes69" -- false

-- getting a category from a character
returnsUppercaseLetter = generalCategory 'A'

_ = map digitToInt "12345" --runtime error if not [0..9] or [aA..fF]


-- encoding using character codes
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted  

_ = encode 3 "Heeeeey"  
-- "Khhhhh|"

-- decode using the encode function
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg  

-- operations on maps

-- map initialization

-- maps are merely lists of tuples
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
        ]  

findFromKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
{-  *Main> findFromKey "betty" phoneBook
    "555-2938" -}

-- actual map representation
mapPhoneBook = Map.fromList phoneBook

-- using sets 
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!" 

set1 = Set.fromList text1
set2 = Set.fromList text2

-- now we have standard set operations available
_ = Set.intersection set1 set2
-- fromList " adefhilmnorstuy"  

_ = Set.union set1 set2  
--  fromList" !.?AIRTabcdefghijlmnorstuvwy"  

-- #### MAKING MODULES #### 
-- see file Geometry.hs