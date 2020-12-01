module Day1 where

import System.IO


alg :: [Int] -> Maybe [Int]
alg [] = Nothing
alg l  = Just [ x*y*z | x <- l,
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

output :: String -> Maybe [Int]
output s = alg (getInput s)
              
main = do
  contents <- readFile "input.txt"
  print $ output  contents

    
