module Day8 where


import System.IO


whereMove :: String -> (String, Int)
whereMove str = (ins, f move)
    where
      wordL = words str
      (ins, move) = (head wordL, wordL !! 1)
      f x = if (head x == '+') then (read (tail x) :: Int) else (read x :: Int)

runInstruction :: String -> (Int, Int) -> (Int, Int)
runInstruction ins (acc, pos)
               | "nop" == op = (acc, pos + 1)
               | "jmp" == op = (acc, pos + arg)
               | "acc" == op =  (acc + arg, pos + 1)
               where
                 (op, arg) = whereMove ins

runInstruction2 :: (String, Int) -> (Int, Int) -> (Int, Int)
runInstruction2 (op, arg) (acc, pos)
               | "nop" == op = (acc, pos + 1)
               | "jmp" == op = (acc, pos + arg)
               | "acc" == op =  (acc + arg, pos + 1)

changen :: a -> Int -> [a] -> [a]
changen x pos list = ls ++ [x] ++ tail lr
    where
      (ls, lr) = splitAt pos list
                             
star1 :: Int -> [String] -> [Int] -> (Int, Int) -> (Int, [Int])
star1 ord (instructions) visited (acc, pos)
      | follow == False = (acc, visited)
      | otherwise = star1 (ord + 1 ) instructions visited_n n_acc_pos
        where
          follow = visited !! pos == -1
          visited_n =  changen ord pos visited 
          n_acc_pos = runInstruction (instructions !! (pos)) (acc, pos)

                      
                      

canIReachLegal :: [(Int, (String, Int))] -> Int -> [(Int, (String, Int))]
canIReachLegal instructions pos = [(ind, (ins, arg)) | (ind, (ins, arg)) <- instructions,
                                                                            (ins == "jmp" && (ind + arg) == pos) ||
                                                                            (ins == "acc" && (ind +1) == pos) ||
                                                                            (ins == "nop" && (ind+1) == pos)
                                  ]
canIReachMod :: [(Int, (String, Int))]-> [Int] -> Int -> [(Int, (String, Int))]
canIReachMod instructions visited pos = [(ind, (ins, arg)) | (ind, (ins, arg)) <- instructions,
                                                                            (isVisited ind visited) ,
                                                                            (ins == "jmp" && (ind + 1) == pos) ||
                                                                            (ins == "nop" && (ind+arg) == pos)
                                  ]

star2 :: [(Int, (String, Int))] -> [Int] -> Int -> [(Int, (String, Int))]
star2 instructions visited target
      | isVisited target visited = [instructions !! target]
      | reachMod /= [] = reachMod
      | reachLeg /= [] = foldl1 (++) (map (star2 instructions visited) (map getIndex reachLeg))
      | otherwise = []
      where
          reachMod =  canIReachMod instructions visited target
          reachLeg = canIReachLegal instructions target
          getIndex (index, _) = index


                                
isVisited :: Int -> [Int] -> Bool
isVisited pos vis = vis !! pos /= -1


compute :: [(String,Int)] -> (Int, Int) -> (Int, Int)
compute (instructions) (acc, pos)
      | pos == (length instructions -1) = runInstruction2 (instructions !! pos) (acc, pos)
      | otherwise = compute instructions n_acc_pos
        where
          n_acc_pos = runInstruction2 (instructions !! (pos)) (acc, pos)

changeIns :: [String] -> (Int, (String, Int)) -> [(String, Int)]
changeIns ins (index, (oins, arg)) = n_inst
              where
                inst = map whereMove ins
                n_inst = if (oins == "jmp") then changen ("nop", arg) index inst
                         else changen ("jmp", arg) index inst
                         
main = do
  content <- readFile "input8.txt"
  let
     instructions = lines content
     (s1, visited) = star1 0 instructions (take (length instructions) (repeat (-1))) (0, 0)
     insmod =  star2 (zip [0..] (map whereMove instructions)) visited (length instructions -1)
     ninst = changeIns instructions (head insmod)
     s2 = compute (ninst) (0, 0)
  print $ s1
  print $ star2 (zip [0..] (map whereMove instructions)) visited (length instructions -1)
  print $ s2
  
