module Day1 where

import System.IO


alg1 :: [Int] -> Maybe [Int]
alg1 [] = Nothing
alg1 l  = Just [ x*y | x <- l,
                      y <- l, 
                      x + y == 2020]

alg2 :: [Int] -> Maybe [Int]
alg2 [] = Nothing
alg2 l  = Just [ x*y*z | x <- l,
                      y <- l,
                      z <- l, 
                      x + y + z == 2020]
    
stringtoInt :: String -> [[Int]]
stringtoInt s = map (map readInt )(map words (lines s))
                where
                  readInt = read :: String -> Int

getInput :: String -> [Int]
getInput s = map f (stringtoInt s)
    where
      f [a] = a

output1 :: String -> Maybe [Int]
output1 s = alg1 (getInput s)

output2 :: String -> Maybe [Int]
output2 s = alg2 (getInput s)             
main = do
  contents <- readFile "input.txt"
  print $ output1  contents
  print $ output2  contents
    
