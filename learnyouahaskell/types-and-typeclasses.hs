-- definition of bool from standard library
data Bool = False | True 


-- defining a shape of Circle | Rectangle
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)


-- matching on types
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  

-- since constructors are functions, constructing a shape looks like a function call
aShape = Circle 10 10 10

-- because constructors are functions we can do the following
circlesWithSameCenter = map (Circle 10 20) [1,2,3,4,5]


-- nesting data types
data Point = Point Float Float deriving (Show)  
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)  

surface' :: Shape' -> Float  
surface' (Circle' _ r) = pi * r ^ 2  
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

translate :: Shape' -> Float -> Float -> Shape'
translate (Circle' (Point x y) r) a b = Circle' (Point (x+a) (y+b)) r
translate (Rectangle' (Point x y) (Point x1 y1)) a b = Rectangle' (Point (x+a) (y+b)) (Point (x1+a) (y1+b))


-- record syntax - named attributes
-- automatically create functions to retrieve individual values for us
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) 

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  

-- also, when using record syntax we can create instances out of order
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  
someCar = Car {model="Mustang", company="Ford", year=1967}  

-- type constructors :: eg. Maybe implementation
-- here maybe can not be on its own
-- Notice 'a' on left side of equals
-- data Maybe a = Nothing | Just a 


-- DON'T USE TYPE CONSTRAINTS IN DATA DECLARATIONS
-- FUNCTIONS WILL HAVE TO ENFORCE THEM EITHER WAY:

-- Left side of '=' is type constructor
-- Right side of '=' is value constructor
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

-- Derived instances
-- This can only derive from Eq since both fields are in Eq
data Truck = Truck { name :: String  
                   , color :: String  
                   } deriving (Show,Eq,Read)  

-- When using read in a context where type can be inferred, no declaration needed
truth = 1 == read "1"

-- When it cannot be inferred, be explicit
bigtruck = read "Truck {name =\"Dr. Huge Truck\", color=\"Big Red\"}" :: Truck

-- Nothing is smaller than any just
-- however when comparing Just with Just, their innards are compared
nothingIsTiny = Nothing < Just (-49999)

-- Typeclasses are crazy - because no Day takes params, derive Enum.
-- Since it's ordered (increaseing value of declaration) we can also derive Bounded
-- ghci> minBound :: Day
-- Monday
-- ghci> succ Monday
-- Tuesday
-- ghci> [Thursday .. Sunday]
-- [Thursday,Friday,Saturday,Sunday]
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  
