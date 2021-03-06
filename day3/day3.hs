module Day3 where

import System.IO


getInput :: String -> [String]
getInput str = lns 
               where lns = lines str


getPos :: (Int, Int) -> (Int, Int)
getPos (n, m) = (n+1, m + 3)

itsTree :: (Int, Int) -> String -> Bool
itsTree (_, m) str = (str !! pos ) == '#'
          where
            pos = mod m (length str)
        
nTrees :: (Int, Int) ->  (Int, Int) -> [String] -> Int -> Int
nTrees (patt1, patt2) (n, m) (xs) cont
       | n >= length xs = cont
       | itsTree (n, m) x = nTrees  (patt1, patt2) (n+patt1, m+patt2) xs (cont + 1)
       | otherwise = nTrees  (patt1, patt2) (n+patt1, m + patt2) xs cont
          where x = xs !! n
                    
main = do
  contents <- readFile "input3.txt"
  let
      inputl = getInput contents
      slp1 = nTrees (1, 1) (0,0) inputl 0
      slp2 = nTrees (1, 3) (0,0) inputl 0
      slp3 = nTrees (1, 5) (0,0) inputl 0
      slp4 = nTrees (1, 7) (0,0) inputl 0
      slp5 = nTrees (2, 1) (0,0) inputl 0
  print $ slp1*slp2*slp3*slp4*slp5


