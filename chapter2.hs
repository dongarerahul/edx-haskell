-- class constraints Num, Eq, Ord
sum' :: Num a => [a] -> a
sum' a = sum a

-- currying convention : right associativity of ->
-- multi a b c implies ((multi a) b) c
-- unless tupling is explicitly required, all functions in haskell
-- are curried by default


-- polymorphic functions
-- len here can take any type of argument
len :: [a] -> Int
len a = length a

-- standard polymorphic functions from prelude

head :: [a] -> a
head (x:xs) = x

-- Guarded Expression
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs)  | n <= 0             = []
                | n >= length (x:xs) = (x:xs)
                | otherwise         = x : take' (n-1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] a = []
zip' a [] = []
zip' (a:as) (b: bs) = (a, b) : (zip' as bs)

palindrome :: Ord a => [a] -> Bool
palindrome a = reverse a == a

--f :: x -> x
--twice :: f -> x
--twice f x = f (f x)
