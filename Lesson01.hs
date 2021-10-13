module Lesson01 where

x = 42 + 1

name :: [Char]
name = "asds"

g :: Int -> Int -> Int
g x y = 
  let 
    add x y = x + y
    in add x y
