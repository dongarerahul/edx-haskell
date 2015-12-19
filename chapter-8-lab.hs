triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n - 1)

count :: Eq a => a -> [a] -> Integer
count k [] = 0
count k (x:xs) = if k == x then 1 + count k xs else count k xs

euclid :: (Integer, Integer) -> Integer
euclid (a, b) | a == b     = a
              | a > b     = euclid (a - b, b)
              | otherwise = euclid (b - a, a)

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]

-- split list into two : (odds, evens)
split :: [a] -> ([a], [a])
split []   = ([], [])
split [x]  = ([x], [])
split (x: y: xs) = (x : xp, y : yp) where (xp, yp) = split xs

-- f applied to even positions, g applied to odd positions
funkyMap f g [] = []
funkyMap f g [x] = [(f x)]
funkyMap f g (x:y:xs) = (f x) : (g y) : (funkyMap f g xs)

