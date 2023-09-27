module Ex_seven where

import Data.List
import Data.Monoid
import Data.Semigroup
import Data.Char

------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)

-- velocity computes a velocity given a distance and a time
velocity :: Distance -> Time -> Velocity
velocity (Distance x) (Time y) = Velocity (x/y)

-- travel computes a distance given a velocity and a time
travel :: Velocity -> Time -> Distance
travel (Velocity x) (Time y) = Distance (x*y)

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
--
-- Examples:
--   member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set a = Set [a]
  deriving (Show,Eq)

-- emptySet is a set with no elements
emptySet :: Set a
emptySet = Set []

-- member tests if an element is in a set
member :: Eq a => a -> Set a -> Bool
member x (Set y) = elem x y

-- add a member to a set
add :: Ord a => a -> Set a -> Set a
add i (Set y) = Set (insert i y)

------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (reperesented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq,Show)

data State = Start | Error | Finished | EggsAdded | EFAdded | ESAdded | BothAdded | Mixed
  deriving (Eq,Show)

step :: State -> Event -> State
step Start AddEggs = EggsAdded
step Start _ = Error
step EggsAdded AddFlour = EFAdded
step EggsAdded AddSugar = ESAdded
step EggsAdded _ = Error
step EFAdded AddSugar = BothAdded
step EFAdded _ = Error
step ESAdded AddFlour = BothAdded
step ESAdded _ = Error
step BothAdded Mix = Mixed
step Mixed Bake = Finished
step Finished _ = Finished

-- do not edit this
bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- PS. The Data.List.NonEmpty type has been imported for you
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

--average :: Fractional a => NonEmpty a -> a
--average = undefined

------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

instance Semigroup Distance where
  (<>) (Distance x) (Distance y) = Distance (x+y)

instance Semigroup Time where
  (<>) (Time x) (Time y) = Time (x+y)

instance Semigroup Velocity where
  (<>) (Velocity x) (Velocity y) = Velocity (x+y)

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?
instance (Eq a, Ord a) => Semigroup (Set a) where
  (<>) (Set x) (Set y) = Set (sort $ nub $ x++y)

instance Ord a => Monoid (Set a) where
  mempty = Set []

------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"

data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int
  deriving Show

compute1 :: Operation1 -> Int
compute1 (Add1 i j) = i+j
compute1 (Subtract1 i j) = i-j
compute1 (Multiply1 i j) = i*j

show1 :: Operation1 -> String
show1 (Add1 i j) = show(i) ++ "+" ++ show(j) ++ "=" ++ show(compute1 (Add1 i j))

data Add2 = Add2 Int Int
  deriving Show
data Subtract2 = Subtract2 Int Int
  deriving Show
data Multiply2 = Multiply2 Int Int
  deriving Show

class Operation2 op where
  compute2 :: op -> Int

instance Operation2 Add2 where
  compute2 (Add2 i j) = i+j

instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i-j

instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i*j

show2 :: Add2 -> String
show2 (Add2 i j) = show(compute2 (Add2 i j)) --etc