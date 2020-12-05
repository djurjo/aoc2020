module Day3 where

import System.IO

takeRow :: String -> [Int] -> Int
takeRow [x] [p1, p2] = if x == 'F' then p1 else p2
takeRow _ [p] = p
takeRow [] pos = pos !! 0
takeRow (x:xs) pos
        | x == 'F' = takeRow xs lower
        | x == 'B' = takeRow xs upper
        where
          (lower, upper) = splitAt (div (length pos) 2) pos
    
takeColumn :: String -> [Int] -> Int
takeColumn [x] [c1, c2] = if x == 'R' then c2 else c1
takeColumn (x:xs) cols
           | x == 'R' = takeColumn xs upper
           | x == 'L' = takeColumn xs lower
           where
             (lower, upper) = splitAt (div (length cols) 2) cols 

takeRowCol :: String -> (Int, Int)
takeRowCol [] = (0, 0)
takeRowCol str = (takeRow r [0..127], takeColumn c [0..7])
    where
      (r, c) = splitAt 7 str
               
                              
id_ :: (Int, Int) -> Int
id_ (row, col) = 8*row + col

star1 :: [String] -> Int
star1 [] = 0
star1 (x:xs) = max (star1 xs) (id_ (rown, coln))
               where
                 (rows, cols) = splitAt 7 x
                 (rown, coln) = (takeRow rows [0..127], takeColumn cols [0..7])               

idlist :: [String] -> [Int]
idlist [] = []
idlist str = map id_ (map takeRowCol str)

missing :: [Int] -> Int           
missing ints =div ((missupp !! 1) +  (missupp !! 0))  2
    where
      missupp = [u | u<- ints , f u ints]
      f x xs = if (not (elem (x - 1) xs))
                  then (elem (x-2) xs)
                  else (    if (not (elem (x + 1) xs))
                               then (elem (x + 2) xs)
                                else False
                       )

main = do
  contents <-   readFile "input5.txt"
  let
      bspart = lines contents
      st1 = star1 bspart
  print $  st1
  print $ missing (idlist bspart)
