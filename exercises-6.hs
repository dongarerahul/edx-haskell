type Bit = Int
int2bin :: Int -> [Bit]

int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- unfold generate a list using predicate, head func, tail func and x seed
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- unfold if predicate p is true, returns empty list
-- otherwise it generates list of which
--   head is generated applying h function on seed x
--   tail is generated by applying t function on seed x and that is passed recursively to unfold

int2bin' :: Int -> [Bit]
int2bin' = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

--chop8' :: [Bit] -> [[Bit]]
--chop8' [] = []
--chop8' = unfold null (take 8) (drop 8)

-- implement map (a -> b) -> [a] -> [b] using unfold
map' f = unfold null (f . head) tail

-- iterate is a function that takes a function and starting value. Then it applies the function to the starting value, then it applies the same function to the last result, and so on.
-- implement iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f
