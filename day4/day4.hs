module Day4 where

import System.IO
import Data.Map as M
import Data.Set as S
    
type Passport = M.Map String String

splitAndConst :: Char -> (String, String) -> (String, String)
splitAndConst x (zs , (y:ys))
              | y == x = (zs, ys)
              | otherwise = splitAndConst x (zs ++ [y], ys)
splitAndConst _ (zs, "") = (zs, "")

aux :: Char -> String -> (String, String)
aux x str = splitAndConst x ("", str)
    
addToPassport :: String -> Passport -> Passport
addToPassport str pss = M.union pss nPss
    where
      sep = words str --- notice that words "a:b c:d" -> ["a:b", "c:d""]
      nPss = M.fromList (Prelude.map f sep)
      f = aux ':'
             
get1 :: ([String], Passport) -> ([String], Passport)
get1 ([] , ps) = ([], ps)
get1 ((x:xs),  ps) 
    | x == "" = (xs, ps)
    | otherwise = get1 (xs ,    ( addToPassport  x  ps  ) )

getAll :: [String] -> [Passport]
getAll [] = []
getAll str = pss : ( getAll nstr) 
    where
      (nstr, pss) = get1 (str, (M.fromList [])) 

start1 :: [Passport] -> (Passport -> Bool) -> Int
start1 pss valid = length (Prelude.filter (valid) pss)

             
isValidPassport1 :: Passport -> Bool
isValidPassport1 ps  =  S.isSubsetOf  (S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) (keysSet ps) 

isValidPassport2 :: Passport -> Bool
isValidPassport2 ps = isValidPassportL pslist
    where
      pslist = M.toList ps
      
isValidPassportL :: [(String, String)] -> Bool
isValidPassportL pass = foldl1 (&&) (Prelude.map validT pass)  



validT :: (String, String) -> Bool
validT (key, value)
      | key == "byr" = validBYR value
      | key == "iyr" = validIYR value
      | key == "eyr" = validEYR value
      | key == "hgt" = validHGT value
      | key == "hcl" = validHCL value
      | key == "ecl" = validECL value
      | key == "pid" = validPID value
      | key == "cid" = True
                                      
validBYR :: String -> Bool
validBYR yr = 1920 <= yrr && yrr <= 2002
    where yrr = read yr::Int

validIYR :: String -> Bool
validIYR yr = 2010 <= yrr && yrr <= 2020
    where yrr = read yr::Int

validEYR :: String -> Bool                
validEYR yr = 2020 <= yrr && yrr <= 2030
    where yrr = read yr::Int   

validHGT :: String -> Bool
validHGT hgt
         | mss == "in" = 59 <= szInt && szInt  <= 76
         | mss == "cm" = 150 <= szInt && szInt  <= 193
         | otherwise = False
    where       
         (sz, mss) = Prelude.splitAt (length hgt - 2) hgt
         szInt = read sz :: Int

validHCL :: String -> Bool
validHCL hcl = (hsg == '#') && (correctType digits 'S') && (length digits == 6)
    where
      hsg = head hcl
      digits = tail hcl

validECL :: String -> Bool
validECL color = elem color ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPID :: String -> Bool
validPID pid = length pid == 9

correctType :: String -> Char -> Bool
correctType str id
           | id == 'D' = foldl1 (&&) (Prelude.map f str)
           | id == 'S' = foldl1 (&&) (Prelude.map g str)
           where
             f x = elem x ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
             g x = elem x ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'a', 'b', 'c', 'd', 'e', 'f']
main = do
  contents <- readFile "input4.txt"
  let
      inputl = lines contents
  print $ start1  (getAll inputl) isValidPassport1
  print $ start1  (Prelude.filter (isValidPassport1) (getAll inputl)) isValidPassport2
