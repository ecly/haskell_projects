import qualified Data.Map as Map
-- definition of bool from standard library
data Bool' = False' | True'


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

-- General type synonym
type AssocList k v = [(k, v)]

-- Partially applied type parameters
type IntMap v = Map.Map Int v
type IntMap' = Map.Map Int

-- Often used for errors - where Left stores the error and Right stores the result of computation.
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- Locker example
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- Test map
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- Recursive data structures (Algebraic data types)
data List' a = Empty' | Cons a (List' a) deriving (Show, Read, Eq, Ord)
someList = 5 `Cons` Empty'

-- We can define infix functions by using only special characters.
-- Since constructors are just functions that return a data type, we can
-- do the same with these.
-- infixr specifies that it is right associative and binds with tightness 5.
-- * is infxl 7 and + is infxl 6
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeContains :: (Ord a) => a -> Tree a -> Bool
treeContains x EmptyTree = False
treeContains x (Node a left right)
    | x == a = True
    | x < a  = treeContains x left
    | x > a  = treeContains x right

-- Building a tree with fold
nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

-- Typeclasses 102
-- Recap: typeclasses are like interfaces that define some behavior (eg. comparing for equality, order etc.)
-- types that behave that way become instances of those typeclasses.
--
-- Because eq == and /= are defined recursively in terms of eachother,
-- we only have to overwrite one of them.
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- Just deriving show would merely have interpreted constructor names as literals.
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- For type constructors , eg. non-concrete we do the follwing
-- Notice how we need to add a type class constraint to be sure
-- that m is in instance of Eq.
-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False

-- :info Maybe (-- use info to see what classes types are instances of --)

-- A yes-no typeclass
-- Javascript weak true/false
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- The Functor typeclass
-- implementation:
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Reminder, Maybe Int is a concrete type, but Maybe is a type constructor that
-- takes one argument.

-- map's type signature:
-- map :: (a -> b) -> [a] -> [b]
--
-- Lists are instances of the functor typeclass the following way:
-- instance Functor [] where
--     fmap = map
--
-- How Maybe is an instance of Functor
-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing
--
-- How Tree is an instance
--     instance Functor Tree where
--        fmap f EmptyTree = EmptyTree
--        fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
--
-- Either's membership in Functor
-- - notice how it gives a, as a function is expected to be a type constructor
--   with one parameter:
--
--    instance Functor (Either a) where
--        fmap f (Right x) = Right (f x)
--        fmap f (Left x) = Left x
--

-- Kinds and some type-foo
--
-- A kind is the type of the type :)
--
-- :k Int
-- Int :: *
-- This means int is a concrete type
--
-- :k Maybe
-- Maybe :: * -> *
-- Means Maybe is a type constructor that takes one concrete type and produces a concrete type.o
--
-- Types that want to be functors should have type * -> *
