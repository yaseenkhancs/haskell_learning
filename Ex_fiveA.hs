module Ex_fiveA where

import Data.List
import Data.Char

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.

data Vehicle = Bike | Bus | Tram | Train
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"

data BusTicket = SingleTicket | MonthlyTicket String
  deriving Show

------------------------------------------------------------------------------
-- Ex 3: Here's the definition for a datatype ShoppingEntry that
-- represents an entry in a shopping basket. It has an item name (a
-- String), an item price (a Double) and a count (an Int). You'll also
-- find two examples of ShoppingEntry values.
--
-- Implement the functions totalPrice and buyOneMore below.

data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

threeApples :: ShoppingEntry
threeApples = MkShoppingEntry "Apple" 0.5 3

twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry "Banana" 1.1 2

-- totalPrice should return the total price for an entry
--
-- Hint: you'll probably need fromIntegral to convert the Int into a
-- Double
--
-- Examples:
--   totalPrice threeApples  ==> 1.5
--   totalPrice twoBananas   ==> 2.2

totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry _ x y) = x * (fromIntegral y)

-- buyOneMore should increment the count in an entry by one
--
-- Example:
--   buyOneMore twoBananas    ==> MkShoppingEntry "Banana" 1.1 3

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry x y z) = (MkShoppingEntry x y (z+1))

------------------------------------------------------------------------------
-- Ex 4: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).

data Person = MkPerson String Int
  deriving Show

-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = MkPerson "Fred" 90

-- getName returns the name of the person
getName :: Person -> String
getName (MkPerson name _) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (MkPerson _ age) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName newname (MkPerson name age) = (MkPerson newname age)

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge newage (MkPerson name age) = (MkPerson name newage)

------------------------------------------------------------------------------
-- Ex 5: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
--
-- Examples:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

data Position = Pos Int Int
  deriving Show

-- origin is a Position value with x and y set to 0
origin :: Position
origin = Pos 0 0

-- getX returns the x of a Position
getX :: Position -> Int
getX (Pos x _) = x

-- getY returns the y of a position
getY :: Position -> Int
getY (Pos _ y) = y

-- up increases the y value of a position by one
up :: Position -> Position
up (Pos x y) = (Pos x (y+1))

-- right increases the x value of a position by one
right :: Position -> Position
right (Pos x y) = (Pos (x+1) y)

------------------------------------------------------------------------------
-- Ex 6: Here's a datatype that represents a student. A student can
-- either be a freshman, a nth year student, or graduated.

data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

-- Implement the function study, which changes a Freshman into a 1st
-- year student, a 1st year student into a 2nd year student, and so
-- on. A 7th year student gets changed to a graduated student. A
-- graduated student stays graduated even if he studies.

study :: Student -> Student
study Freshman = NthYear 1
study (NthYear x)
  | x < 7 = NthYear (x+1)
  | otherwise = Graduated
study Graduated = Graduated

------------------------------------------------------------------------------
-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in increasing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype (feel free to name the
-- constructors however you want)
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Up Int | Down Int
  deriving Show

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Up 0

-- get returns the counter value
get :: UpDown -> Int
get (Up x) = x
get (Down x) = x

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Up x) = Up (x+1)
tick (Down x) = Down (x-1)

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Up x) = (Down x)
toggle (Down x) = (Up x)

------------------------------------------------------------------------------
-- Ex 8: you'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Invert.
--
-- Mix means the average of the two colors in each rgb channel.
--
-- Invert means subtracting all rgb values from 1.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                    ==> [0.5,0.5,0]
-- rgb (Mix Red (Mix Red Green))          ==> [0.75,0.25,0]
-- rgb (Invert Red)                       ==> [0,1,1]
-- rgb (Invert (Mix Red (Mix Red Green))) ==> [0.25,0.75,1]
-- rgb (Mix (Invert Red) (Invert Green))  ==> [0.5,0.5,1]

data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

rgb :: Color -> [Double]
rgb Red = [1, 0, 0]
rgb Green = [0, 1, 0]
rgb Blue = [0, 0, 1]
rgb (Mix x y) = [(((rgb x) !! 0) + ((rgb y) !! 0)) / 2, (((rgb x) !! 1) + ((rgb y) !! 1)) / 2, (((rgb x) !! 2) + ((rgb y) !! 2)) / 2]
rgb (Invert x) = [(1- (rgb x !! 0)), (1- (rgb x !! 1)), (1- (rgb x !! 2))]

------------------------------------------------------------------------------
-- Ex 9: define a parameterized datatype OneOrTwo that contains one or
-- two values of the given type. The constructors should be called One and Two.
--
-- Examples:
--   One True         ::  OneOrTwo Bool
--   Two "cat" "dog"  ::  OneOrTwo String

data OneOrTwo a = One a | Two a a
  deriving Show

------------------------------------------------------------------------------
-- Ex 10: define a recursive datatype KeyVals for storing a set of
-- key-value pairs. There should be two constructors: Empty and Pair.
--
-- Empty represents an empty collection. It should have no fields.
--
-- Pair should have three fields, one for the key, one for the value,
-- and one for the rest of the collection (of type KeyVals)
--
-- The KeyVals datatype is parameterized by the key type k and
-- the value type v.
--
-- For example:
--
--  Pair "cat" True (Pair "dog" False Empty)  ::  KeyVals String Bool
--
-- Also define the functions toList and fromList that convert between
-- KeyVals and lists of pairs.

data KeyVals k v = Empty | Pair k v (KeyVals k v)
  deriving Show

toList :: KeyVals k v -> [(k,v)]
toList Empty = []
toList (Pair k v rest) = (k, v) : toList rest

fromList :: [(k,v)] -> KeyVals k v
fromList [] = Empty
fromList ((k, v):xs) = Pair k v (fromList xs)

------------------------------------------------------------------------------
-- Ex 11: The data type Nat is the so called Peano
-- representation for natural numbers. Define functions fromNat and
-- toNat that convert natural numbers to Ints and vice versa.
--
-- Examples:
--   fromNat (PlusOne (PlusOne (PlusOne Zero)))  ==>  3
--   toNat 3    ==> Just (PlusOne (PlusOne (PlusOne Zero)))
--   toNat (-3) ==> Nothing
--

data Nat = Zero | PlusOne Nat
  deriving (Show,Eq)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne x) = 1 + fromNat x

toNat :: Int -> Maybe Nat
toNat z
  | z < 0 = Nothing
  | z == 0 = Just Zero
  | otherwise = Just (PlusOne (concv (toNat (z-1))))

concv (Just x) = x

