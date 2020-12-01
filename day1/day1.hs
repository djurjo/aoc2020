module Day1 where

import System.IO


alg2 :: [Int] -> Maybe [Int]
alg2 [] = Nothing
alg2 l  = Just [ x*y*z | x <- l,
                      y <- l,
                      z <- l, 
                      x + y + z == 2020]

alg1 :: [Int] -> Maybe [Int]
alg1 [] = Nothing
alg1 l  = Just [ x*y | x <- l,
                      y <- l, 
                      x + y == 2020]
    
stringtoInt :: String -> [[Int]]
stringtoInt s = map (map readInt )(map words (lines s))
                where
                  readInt = read :: String -> Int

getInput :: String -> [Int]
getInput s = map f (stringtoInt s)
    where
      f [a] = a

--output :: ([Int] -> Maybe [Int]) ->[Int] -> Maybe [Int]
--output alg l = alg l
              
main = do
  contents <- readFile "input.txt"
  let
      inputl = getInput contents
  print $ alg1 inputl
  print $ alg2 inputl

    
