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
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True


