-- listIncr1: Increment integers in a list.
listIncr1 :: [Int] -> [Int]
listIncr1 [] = []
listIncr1 (x:xs) = [x+1] ++ listIncr1 xs

-- listIncrN: Increment integers in a list by n.
--listIncrN :: [Int] -> Int -> [Int]
--listIncrN [] _ = []
--listIncrN (x:xs) n = [x+n] ++ listIncrN xs n

-- digitsList: Create list from digits of an integer. 1234 -> [4, 3, 2, 1]
digitsList :: Int -> [Int]
digitsList n = digitsListTail n []
digitsListTail n acc = 
  if n <= 0 then acc        -- base case
  else digitsListTail (div n 10) (mod n 10 : acc)
-- or last line: else digitsListTail (div n 10) ([mod n 10] ++ acc)
{-
digitsList n = 
  if n <= 0 then []         -- Return an empty list for non-positive integers
  else digitsList (div n 10) ++ [mod n 10]
 or:
digitsList n
  | n <= 0    = []         -- Return an empty list for non-positive integers
  | otherwise =  digitsList (div n 10) ++ [mod n 10]
-}

-- listDiv: Create list from numbers divisible by n of a list
listDiv :: [Int] -> Int -> [Int]
listDiv [] _ = []
listDiv (x:xs) n = 
    if mod x n == 0 then [x] ++ listDiv xs n
    else listDiv xs n
{- or:
listDiv [] _ = []
listDiv (x:xs) n
    | mod x n == 0 = [x] ++ listDiv xs n
    | otherwise    = listDiv xs n
-}

main = do
  putStrLn "CS303 Hw-1:"

  putStrLn "Part-1:"
  let a = [2, 1, 7, 6, 3, 8, 4]
  putStrLn "List:"
  print a
  let b = listIncr1 a
  putStrLn "Incremented list:"
  print b
  putStrLn "Incremented empty list:"
  putStrLn (show (listIncr1 []))

--  putStrLn ("Incremented list by 10: " ++ show (listIncrN a 10))

  putStrLn "\nPart-2:"
  let n = 20714
  let ndgt = digitsList n
  putStrLn ("Digits of " ++ show n ++ " are " ++ show ndgt)
  let m = -20714
  let mdgt = digitsList m
  putStrLn ("Digits of " ++ show m ++ " are " ++ show mdgt)

  putStrLn "\nPart-3:"
  let a = [24..44]
  putStrLn "List:"
  print a
  let b = listDiv a 7
  putStrLn "Elements divisible by 7:"
  print b

  let d = listDiv [] 7
  putStrLn "Elements divisible by 7 in empty list:"
  print d
