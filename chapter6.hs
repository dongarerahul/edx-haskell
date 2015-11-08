import Data.Char

--higher order fuction: add 1 twice
--twice (\ a -> a + 1) 5 or twice (+1) 5
twice :: (a -> a) -> a -> a
twice f a = f(f a)

map' :: (a -> b) -> [a] -> [b]
map'   f a = [f x | x <- a]

map''  f [] = []
map''  f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

filter'' f [] = []
filter'' f (x: xs)
  | f x = x : filter'' f xs
  | otherwise = filter'' f xs

-- sum of list
sum' [] = 0
sum' (x:xs) = x + sum xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) =  f x (foldr' f v xs)

sum'' = foldr (+) 0  -- find sum of list
--length' = foldr' (\ a -> 1 + a) 0

-- . operator returns combination of two functions
--  (.) :: (b -> c) -> (a -> b) -> (a -> c)
odd' = not . even

-- $ operator is used to avoid paranthesis

z = any isSpace "Abc Z"
z' = all isSpace "   "

-- map and filter functions using foldr
mapp f xs = foldl (\ acc x -> acc ++ [f x] ) [] xs
filterr p xs = foldl (\ acc x -> if(p x) then acc ++ [x] else acc) [] xs

-- dec2int :: Int a => [a] -> a
-- dec2int (x:xs) = fold:
