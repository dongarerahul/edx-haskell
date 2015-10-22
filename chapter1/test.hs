double x = x * x

quad x = double (double x)

factorial x = x * factorial (x - 1)

factorial1 x = product [1 .. x]

average x = sum x `div` length x -- syntactic sugar

f n = a `div` (length xs)
    where
      a = 10
      xs = [1 .. 5]

last1 xs = drop (length xs - 1) xs

init1 xs = take len xs
             where len = length xs - 1

init2 [] = []
init2 [x] = []
init2 (x : xs : []) = [x]
init2 (x : xs) = x : init2 xs


