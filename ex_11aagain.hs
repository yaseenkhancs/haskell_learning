module Exelvaagain where

import Data.List
import Data.Char

------------------------------------------------------------------------------
-- Ex 1: define an IO operation hello that prints two lines. The
-- first line should be HELLO and the second one WORLD

hello :: IO ()
hello = do
  putStrLn("HELLO")
  putStrLn("WORLD")

------------------------------------------------------------------------------
-- Ex 2: define the IO operation greet that takes a name as an
-- argument and prints a line "HELLO name".

greet :: String -> IO ()
greet name = do
  putStrLn("HELLO " ++ name)

------------------------------------------------------------------------------
-- Ex 3: define the IO operation greet2 that reads a name from the
-- keyboard and then greets that name like the in the previous
-- exercise.
--
-- Try to use the greet operation in your solution.

greet2 :: IO ()
greet2 = do
  x <- getLine
  putStrLn("HELLO " ++ x)

------------------------------------------------------------------------------
-- Ex 4: define the IO operation readWords n which reads n lines from
-- the user and produces them as a list, in alphabetical order.
--
-- Example in GHCi:
--   Set11> readWords 3
--   bob
--   alice
--   carl
--   ["alice","bob","carl"]

readWords :: Int -> IO [String]
readWords 0 = return []
readWords n = do
  x <- getLine
  s <- readWords(n-1)
  return (x : s)

------------------------------------------------------------------------------
-- Ex 5: define the IO operation readUntil f, which reads lines from
-- the user and returns them as a list. Reading is stopped when f
-- returns True for a line. (The value for which f returns True is not
-- returned.)
--
-- Example in GHCi:
--   *Set11> readUntil (=="STOP")
--   bananas
--   garlic
--   pakchoi
--   STOP
--   ["bananas","garlic","pakchoi"]

readUntil :: (String -> Bool) -> IO [String]
readUntil f = do
  x <- getLine
  if (not (f(x)))
    then do
      s <- readUntil f
      return (x:s)
    else do return []

------------------------------------------------------------------------------
-- Ex 6: given n, print the numbers from n to 0, one per line

countdownPrint :: Int -> IO ()
countdownPrint 0 = do print 0
countdownPrint n = do
  print n
  countdownPrint (n-1)

------------------------------------------------------------------------------
-- Ex 7: isums n should read n numbers from the user (one per line) and
--   1) after each number, print the running sum up to that number
--   2) finally, produce the sum of all numbers
--
-- Example:
--   1. run `isums 3`
--   2. user enters '3', should print '3'
--   3. user enters '5', should print '8' (3+5)
--   4. user enters '1', should print '9' (3+5+1)
--   5. produces 9

isums :: Int -> IO Int
isums n = isums' n 0

isums' :: Int -> Int -> IO Int
isums' 0 total = return total
isums' n total = do
  x <- readLn
  print(x+total)
  isums' (n-1) (x+total)

------------------------------------------------------------------------------
-- Ex 8: when is a useful function, but its first argument has type
-- Bool. Write a function that behaves similarly but the first
-- argument has type IO Bool.

whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
  c <- cond
  if c then op else return ()

------------------------------------------------------------------------------
-- Ex 9: implement the while loop. while condition operation should
-- run operation as long as condition returns True.
--
-- Examples:
--   -- prints nothing
--   while (return False) (putStrLn "IMPOSSIBLE")
--
--   -- prints YAY! as long as the user keeps answering Y
--   while ask (putStrLn "YAY!")

-- used in an example
ask :: IO Bool
ask = do putStrLn "Y/N?"
         line <- getLine
         return $ line == "Y"

while :: IO Bool -> IO () -> IO ()
while cond op = do
  c <- cond
  if c then do
    op
    while cond op
  else
    return ()

------------------------------------------------------------------------------
-- Ex 10: given a string and an IO operation, print the string, run
-- the IO operation, print the string again, and finally return what
-- the operation returned.
--
-- Note! the operation should be run only once
--
-- Examples:
--   debug "CIAO" (return 3)
--     - prints two lines that contain CIAO
--     - returns the value 3
--   debug "BOOM" getLine
--     1. prints "BOOM"
--     2. reads a line from the user
--     3. prints "BOOM"
--     4. returns the line read from the user

debug :: String -> IO a -> IO a
debug s op = do
  putStrLn(s)
  x <- op 
  putStrLn(s)
  return x

--11B

------------------------------------------------------------------------------
-- Ex 7: In this exercise we see how a program can be split into a
-- pure part that does all of the work, and a simple IO wrapper that
-- drives the pure logic.
--
-- Implement the function interact' that takes a pure function f of
-- type
--   (String, st) -> (Bool, String, st)
-- and a starting state of type st and returns an IO operation of type
-- IO st
--
-- interact' should read a line from the user, feed the line and the
-- current state to f. f then returns a boolean, a string to print and
-- a new state. The string is printed, and if the boolean is True, we
-- continue running with the new state. If the boolean is False, the
-- execution has ended and the state should be returned.
--
-- Example:
--   *Set11b> interact' counter 1
--   print
--   1
--   inc
--   done
--   inc
--   done
--   print
--   3
--   quit
--   bye bye
--   3
--   *Set11b>

-- This is used in the example above. Don't change it!
counter :: (String,Integer) -> (Bool,String,Integer)
counter ("inc",n)   = (True,"done",n+1)
counter ("print",n) = (True,show n,n)
counter ("quit",n)  = (False,"bye bye",n)

interact' :: ((String,st) -> (Bool,String,st)) -> st -> IO st
interact' f state = do
  x <- getLine
  if (extractfirst(f (x, state))) then do
    interact' f (extractthird(f (x, state)))
  else do
    return (extractthird(f (x, state)))

extractfirst :: (a, b, c) -> a
extractfirst (a, b, c) = a

extractsecond :: (a, b, c) -> b
extractsecond (a, b, c) = b

extractthird :: (a, b, c) -> c
extractthird (a, b, c) = c