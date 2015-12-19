module Main (main) where
import Data.Char
import Data.List
--import Hugs.IOExts (unsafeCoerce)
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

natToInteger' = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1: m n)]]

--genericLength gives back Num a instead of Integer
--which can be used for / (division) operator
natToInteger'1 :: Nat -> Integer
natToInteger'1 = \ n -> genericLength [c | c <- show n, c == 'S']

-- Note change in signature below Int in-place of Integer
natToInteger'2 :: Nat -> Int
natToInteger'2 = \ n -> length [c | c <- show n, c == 'S']

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))

--integerToNat' n = product [(unsafeCoerce c) :: Integer | c <- show n]
{-
integerToNat'1 :: Integer -> Nat
integerToNat'1 0 = Zero
integerToNat'1 (n+1) = Succ (integerToNat'1 n)

  integerToNat' = head . m
  where {
    ; m 0 = [0]
    ; m (n + 1) = [sum [x | x <- (1 : m n)]]
  }
integerToNat' = \ n -> genericLength [c | c <- show n, isDigit c]
-}
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add n m)

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)

multi :: Nat -> Nat -> Nat
multi m Zero = Zero
multi m (Succ n) = add' m (multi m n)
---------------------

--data Ordering = LT | EQ | GT
data Tree = Leaf Integer | Node Tree Integer Tree

--compare :: (Ord a) => a -> a -> Ordering
--compare a b = if a == b then EQ else if a > b then GT else LT

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = case compare m n of
                             LT -> occurs m l
                             EQ -> True
                             GT -> occurs m r

occurs' m (Leaf n) = m == n
occurs' m (Node l n r)
  | m == n     = True
  | m < n     = occurs' m l
  | otherwise = occurs' m r
------------------------
data Tree' = Leaf' Integer | Node' Tree' Tree' deriving Show
-- tree is balanced if left & right nodes count differ by atmost 1

leaves' :: Num a => Tree' -> a
leaves' (Leaf' _) = 1
leaves' (Node' l r) = leaves' l + leaves' r

balanced :: Tree' -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves' l - leaves' r) <= 1 &&
                        balanced l &&
                        balanced r
------------------------
-- populate integer array into balanced binary tree
balance :: [Integer] -> Tree'
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf' x
balance xs = Node' (balance ys) (balance zs)
  where (ys, zs) = halve xs
-----------



main = do
  print "Hello"
  let one = Succ Zero
  let two = Succ $ Succ Zero
  let three = Succ $ Succ $ Succ Zero
  print three

