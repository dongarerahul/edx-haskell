-- Caesar cipher program

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(n + ord 'a')

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2let ((let2int c + n))
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [shift (-n) x | x <- xs]

-----------------
-- Reverse Sort
-- ----------------------
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
  where smaller = [a | a <- xs, a < x]
        larger  = [a | a <- xs, a > x]

---------- Merge Sort
merge :: Ord a =>  ([a], [a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge ((x:xs), (y:ys)) =
  if x <= y then x : merge (xs, (y: ys)) else y : merge ((x: xs), ys)

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge ((msort yss), (msort xss))
  where (xss, yss) = halve xs

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = xs !! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys
