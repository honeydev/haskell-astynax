module FizzBuzz where
import Data.List
fizzBuzzIf :: Int -> String
fizzBuzzIf n = if n `mod` 3 == 0 && n `mod` 5 == 0
  then "FizzBuzz"
  else
    if n `mod` 3 == 0
    then "Fizz"
    else
      if n `mod` 5 == 0
      then "Buzz"
      else ""

fizzLogic :: Int -> String
fizzLogic n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
fizzLogic n = show n

fizzBuzz :: Int -> String
fizzBuzz n = intercalate "\n" (map fizzLogic [0..n])
