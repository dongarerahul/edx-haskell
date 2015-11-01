import Prelude hiding ((||))

-- lambda expression
odds :: Int -> [Int]
odds n = map f [0..n-1]
  where
    f x = x * 2 + 1

--pattern matching
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

-- guarded equations
safetail' :: [a] -> [a]
safetail' a | null a    = []
            | otherwise = tail a

-- conditional expression
safetail'' :: [a] -> [a]
safetail'' a = if length a == 0 then [] else tail a

-- 3 def for logical or (||) using pattern matching
-- || :: Bool -> Bool -> Bool
--False || False = False
--_ || _ = True

a || b
  | a == b = a
  | otherwise = True

--False || a = a
--True || _ = True

-- and operator
a && b = if a then b else False
a && b = if b then a else False


remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs



