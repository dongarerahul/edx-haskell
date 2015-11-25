module ParserModule where
import Data.Char

newtype Parser a = P (String -> [(a, String)])

------------- Monad World
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Monad Parser where
  return a = P (\ inp -> [(a, inp)])

  p >>= f = P (\ inp -> case (parse p inp) of
                        [(v, out)] -> parse (f v) out
                        [] -> [])
-- Basic Parsers
item :: Parser Char
item = P(\ inp -> case inp of
                   [] -> []
                   (x:xs) -> [(x, xs)])

failure :: Parser Char
failure = P (\ inp -> [])

-- Choice
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P(\ inp -> case parse p inp of
                        [] -> parse q inp -- first fails, proceed to 2
                        [(v, out)] -> [(v, out)]) -- first pass, return

-- parser satisfying a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
              then return x
              else failure

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum =  sat isAlphaNum

-- character matching
char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

--Applying a parser zero or more times
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

--Applying a parser one or more times
many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v: vs)

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             return ()

-- parses one or more integers
nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

int = (do char '-'
          n <- nat
          return (-n))
      +++ nat

--wrong one
--int' = char '-' >>= (\ c -> nat >>= (\ n -> (return (-n) +++ nat)))

space :: Parser ()
space = do many (sat isSpace)
           return ()

--ignore spacing and create token
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- identifier starting with lower letter followed by alphanum
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- ident without spaces
identifier :: Parser String
identifier = token ident

expr :: Parser Int
expr = do n <- natural
          ns <- many
                  (do symbol "-"
                      natural)
          return (foldl (-) n ns)

{--
eval :: String -> Int
eval xs = fst (head(parse expr xs))
--}
--https://github.com/lpil/learning-haskell/blob/master/edx-fp101x/07-hw.hs
