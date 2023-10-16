module Ex_eleven_chapter where

import Data.Char
import Data.List

questionnaire = do
  putStrLn "write something!"
  s <- getLine
  putStrLn ("You wrote: "++s)

query :: IO ()
query = do
  putStrLn "write something"
  s <- getLine
  let n = length s
  putStrLn (show n)

askforaline :: IO String
askforaline = do
  putStrLn "Please give me a line"
  getLine

ask :: String -> IO String
ask question = do
  putStrLn question
  getLine

produceThree :: IO Int
produceThree = return 3

printThree :: IO ()
printThree = do
  three <- produceThree
  putStrLn (show three)

yesNoQuestion :: String -> IO Bool
yesNoQuestion question = do
  putStrLn question
  s <- getLine
  return (s=="Y")