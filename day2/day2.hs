module Day1 where

import System.IO


splitBychar :: (String,String) -> Char -> (String, String)
splitBychar (ys, []) _ = (ys, [])
splitBychar (ys, (x:xs)) t
            | x == t = (ys, xs)
            | otherwise = splitBychar ((ys ++ [x]),  xs) t
                          
splitB :: Char -> String -> (String, String)
splitB t str = splitBychar ([], str) t

policy :: (String, String) -> (Int, Int, String, String)
policy (str, pss) = (read lw :: Int, read up :: Int, key, pss)
    where
      (ints , key) = splitB ' ' str
      (lw, up) = splitB '-' ints
               
                     
takeData :: String -> [(Int, Int, String, String)]
takeData text = data_
    where
      lns = lines text
      input = map (splitB ':') lns
      data_= map (policy) input

count :: Eq a => a -> [a] -> Int
count x xs = length ( filter (x ==) xs)

isValid1 :: (Int, Int, String, String) -> Bool
isValid1 (lw, up, k, pass) = (lw <= n_k) && (n_k <= up)
    where
      n_k = count (k !! 0) pass
       
isValid2 :: (Int, Int, String, String) -> Bool
isValid2 (ind1, ind2, k , pass) = (b1 && (not b2)) ||
                                                  ((not b1) && b2)
         where
           k_c = k!!0 ---boy....
           b1 =  ((pass !! (ind1) == k_c) && (pass !! (ind2) /= k_c ) )
           b2 =  ((pass !! (ind1) /= k_c) && (pass !! (ind2) == k_c ) ) 
             
main = do
  contents <- readFile "input2.txt"
  print $ length (filter (isValid1) (takeData contents))
  print $ filter (isValid2) (takeData contents)
  print $ length (filter (isValid2) (takeData contents))
        --takeData contents
--  print $ alg2 inputl
