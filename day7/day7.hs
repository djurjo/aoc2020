module Day7 where

import System.IO
import Data.Char
import Data.Map as M
import Data.Set as S
    
type Bag = (String,  [(Int, String)])

bagBags :: Bag -> Set String
bagBags (_, bags) = S.fromList (Prelude.map getName (bags))
    where
      getName (_, v) = v
                            
indexA :: Eq a => a -> Int -> [a] -> Int
indexA element pos (x:xs)
      | x == element = pos
      | otherwise = indexA element (pos + 1) xs 
indexA _ _ _ = -1

               
index :: Eq a => a -> [a] -> Int
index element xs = indexA element 0 xs

allIndex :: Eq a => a -> [a] -> [Int]
allIndex element xs
         | pos == -1 = []
         | otherwise = pos : (allIndex element (tail sideR))
           where
             pos = index element xs 
             (sideL, sideR) = Prelude.splitAt pos xs

splitByComma :: String -> [Int] -> [(Int , String)]
splitByComma str (i:is) = (n, bag) : (splitByComma ((tail right)) is )
    where
      (left, right) = Prelude.splitAt i str
      (n, bag) = (read((words left)!!0)::Int,  unwords ( words (tail(tail left))))
                 
splitByComma str [] = [(read((words woPoint) !! 0)::Int,  unwords (words(tail (tail woPoint))) )]
    where
      woPoint = [u | u <- str, u /= '.']
                
getValue :: String -> [(Int,String)]
getValue str
         | wordsL !! 0 == "no" = []
         | otherwise  = splitByComma str indexL
    where
            wordsL = words str
            indexL = allIndex ',' str

                      
splitString :: String -> Bag
splitString str =(key, getValue values)
    where
      key =  unwords  sideL 
      values = unwords (tail sideR)
      (sideL, sideR) = Prelude.splitAt contain wordL 
      contain = index "contain" wordL
      wordL = words str
              
         
getBags :: [String] -> [Bag]
getBags [] = []
getBags (x:xs) = (splitString x): (getBags xs)


           
containsB :: Map String [(Int, String)] -> [String] -> (Int, [String])
containsB bags name = (length news, S.toList (S.union (S.fromList name) (S.fromList news)))
                      where
                        news = [key |  (key, value)<- (M.toList bags),
                                                          not (S.null (S.intersection (bagBags (key, value)) (S.fromList name)))]

                 -- A fixed point computation lmao     
star1 :: Map String [(Int, String)] -> [String] -> Int
star1 bags poss
    | S.fromList poss ==S.fromList new = val
    | otherwise = star1 bags new
    where
      (val, new) = containsB bags poss

    
removeBagAndBags :: Bag -> Bag
removeBagAndBags (key,value) = ( f key ,
                               Prelude.map (g) value)
    where
      f w =unwords ( Prelude.filter (/= "bag") (Prelude.filter (/= "bags") (words w)))
      g (a, b) =(a, f b)



contains :: Map String [(Int, String)] -> (String, [(Int, String)]) -> Int
contains bags (key, value)
         | value  == [] = 0
         | otherwise = sum (Prelude.map f value)
         where
           f (q , k) =q + q * ( contains bags  (k, bags ! k))
main = do
  contents <- readFile "input7.txt"
  let
      bags = M.fromList (Prelude.map removeBagAndBags (getBags (lines contents)))
  print $ star1 bags ["shiny gold"]
  print $ (contains bags ("shiny gold", bags ! "shiny gold")) 
