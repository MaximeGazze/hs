import Data.List

-- main :: IO ()
-- main = putStrLn "Hello, World!"

-- let in
inRange min max value =
  let
    inLow = min <= value
    inUp = max >= value
  in
    inLow && inUp

-- where
inRange2 min max value = inLow && inUp
  where
    inLow = min <= value
    inUp = max >= value

-- if then else
inRange3 min max value =
  if inLow then inUp else False
  where
    inLow = min <= value
    inUp = max >= value

-- Recursion
factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

-- Guards
factorial2 n
  | n <= 1 = 1
  | otherwise = n * factorial2 (n - 1)

-- Multiple definitions
isZero 0 = True
isZero _ = False -- wildcard _

-- Accumulators
fac n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n - 1) (n * acc)

-- def fac(n):
--   acc = 1
--   def aux(n, acc):
--     if (n <= 1):
--       return acc
--     else:
--       return aux(n-1, n*acc)
--   return aux(n, acc)

-- Fizzbuzz
fizzbuzz :: Int -> String
fizzbuzz n = aux 0 n ""
  where
    aux cur n acc
      | cur > n = acc
      | cur == 0 = aux (cur + 1) n (acc)
      | cur `mod` 3 == 0 && cur `mod` 5 == 0 = aux (cur + 1) n (acc ++ "fizzbuzz\n")
      | cur `mod` 3 == 0 = aux (cur + 1) n (acc ++ "fizz\n")
      | cur `mod` 5 == 0 = aux (cur + 1) n (acc ++ "buzz\n")
      | otherwise = aux (cur + 1) n (acc)

-- main :: IO ()
-- main = putStrLn (fizzbuzz 15)

-- Lists
-- list = [1, 2, 3, 4, 5]
-- list2 = 1 : 2 : 3 : 4 : 5 : []

listMan01 = head [1, 2, 3, 4, 5]              -- => 1
listMan02 = tail [1, 2, 3, 4, 5]              -- => [2, 3, 4, 5]
listMan03 = length [1, 2, 3, 4, 5]            -- => 5
listMan04 = init [1, 2, 3, 4, 5]              -- => [1, 2, 3, 4]
listMan05 = null []                           -- => True
listMan06 = null [1, 2, 3, 4, 5]              -- => False
listMan07 = and [True, False]                 -- -> False
listMan08 = or [True, False]                  -- => True

-- List comprehensions
listMan09 = [ 2 * x | x <- [1, 2, 3] ]        -- => [2, 4, 6]
listMan10 = [ 2 * x | x <- [1, 2, 3], x > 1 ] -- => [4, 6]
listMan11 = [ (x, y) | x <- [1, 2, 3], y <- ['a', 'b'] ] -- => [(1, 'a'), (1, 'b'), (2, 'a'), ...]

-- List patterns
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
  | x `mod` 2 == 0 = x : evens xs
  | otherwise = evens xs

-- Tuples
tuple :: (Int, Int)
tuple = (1, 2)
xVal = fst tuple -- => 1
yVal = snd tuple -- => 2

-- let (x, y) = (1, 2) in x
--   => 1

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x + y | (x, y) <- xs ]
addedTuples = addTuples [(1, 2), (2, 3)] -- => [3, 5]

-- elemIn - return True if the given element is in the list, False otherwise
elemIn :: (Eq a) => a -> [a] -> Bool
elemIn _ [] = False
elemIn e (x:xs) = e == x || elemIn e xs

-- nubIn - returns a list with no duplicates
nubIn :: (Eq a) => [a] -> [a]
nubIn [] = []
nubIn (x:xs)
  | elem x xs = nubIn xs
  | otherwise = x : nubIn xs

-- isAsc - returns True if the list is ascending, False otherwise
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:x2:xs) = (x <= x2) && isAsc (x2:xs)

-- hasPath - returns True if a path exists between 2 "nodes" of a tuple. False otherwise
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] currentNode endNode = currentNode == endNode
hasPath (x:y:xs) currentNode endNode
  | currentNode == endNode = True
  | xs == [] = False
  | snd x == fst y = hasPath (y:xs) (snd y) endNode
  | otherwise = False

hasPath2 :: [(Int, Int)] -> Int -> Int -> Bool
hasPath2 [] currentNode endNode = currentNode == endNode
hasPath2 xs currentNode endNode
  | currentNode == endNode = True
  | otherwise =
    let xs' = [ (first, second) | (first, second) <- xs, first /= currentNode ] in
    or [ hasPath2 xs' second endNode | (first, second) <- xs, first == currentNode ]

pow :: Int -> Int -> Int
pow n e = aux n e 1
  where
    aux n e acc
      | e <= 0 = acc
      | otherwise = aux n (e - 1) (acc * n)

pow2 :: Int -> Int -> Int
pow2 n e =
  let
    aux _ 0 acc = acc
    aux n e acc = aux n (e - 1) (acc * n)
  in
    aux n e 1

main = do
  print (pow2 8 0)
  print (pow2 8 1)
  print (pow2 8 2)
