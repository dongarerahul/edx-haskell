squaresList n = [i ^ 2 | i <- [1..n]]

pairsList x y = [(a, b) | a <- [1..x], b <- [10..y]]

-- nested two loop
concat xss = [x | xs <- xss, x <- xs]

-- loop with guard
evens n = [x | x <- [1..n], even x]

factors n = [x | x <- [1..n], n `mod` x == 0]

prime n = factors n == [1, n]

primes n = [x | x <- [1..n], prime x]

pairs a b = [(x, y) | x <- a, y <- b]

pairsOfAdj xs = zip xs (tail xs)

qsort :: Ord a => [a] -> [a]

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

isSorted :: Ord a => [a] -> Bool
isSorted xs = and [x <= y | (x, y) <- pairsOfAdj xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (i, n) <- zip [1..n] xs, n == x]
  where n = length xs

--lowers xs = length [x | x <- xs, isLower x]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

-- no is perfect if sum of its factor = number itself
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect a = sum (init(factors a)) == a

divides :: Int -> Int -> Bool
divides n a = n `mod` a == 0

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], divides n x]

riffle a b = Main.concat [[x, y] | (x, y) <- zip a b]

