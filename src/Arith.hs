module Arith where 

import Text.Printf (printf)
import Data.Char (isDigit, digitToInt)
import qualified Sum (parseNum, splitOn) 

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

-- "1+2*3+4*2" -> 15
data Operator = Plus 
              | Mult 
              | Minus 
              | Div
              deriving (Eq)

data AST = BinOp Operator AST AST 
         | Num  Int 
         deriving (Eq)

-- Преобразует дерево в строку
-- Между числами и знаками операций по одному пробелу
-- BinOp Plus (Num 13) (Num 42) -> "13 42 +"
toPostfix :: AST -> String
toPostfix (Num a) = show a
toPostfix (BinOp op a1 a2) = (toPostfix a1) ++ " " ++ (toPostfix a2) ++ " " ++ (case op of
                                                                                Plus  -> "+"
                                                                                Mult  -> "*"
                                                                                Minus -> "-"
                                                                                Div   -> "/")

-- Парсит выражение в постфиксной записи 
-- Выражение принимается только целиком (не максимально длинный префикс)
-- Между числами и знаками операций по одному пробелу
-- "13 42 +" -> Just (BinOp Plus (Num 13) (Num 42))
-- "1 2 3 +" -> Nothing
-- "1 2 + *" -> Nothing 
fromPostfix :: String -> Maybe AST 
fromPostfix input = do
      [ast] <- (fromPostfix' input [])
      return ast where
        fromPostfix' :: String -> [AST] -> Maybe [AST]
        fromPostfix' "" stack = Just stack
        fromPostfix' (x:xs) stack | x == ' '        = fromPostfix' xs stack
                                  | isDigit x       = do {
                                                       (num, rest) <- parseNum (x:xs);
                                                       fromPostfix' rest (num:stack);
                                                     }
                                  | (elem x "+*/-") = do {
                                                       (op, rest''')  <- parseOp (x:xs);
                                                       (case (stack) of
                                                           (ast1:ast2:rest') -> fromPostfix' rest''' ((BinOp op ast2 ast1) : rest')
                                                           _ -> Nothing
                                                       );
                                                     }
                                  | otherwise       = Nothing



-- Парсит левую скобку
parseLbr :: String -> Maybe (Bool, String)
parseLbr ('(':xs) = Just (True, xs)
parseLbr x        = Just(False, x)

-- Парсит правую скобку
parseRbr :: String -> Maybe (Bool, String)
parseRbr (')':xs) = Just (True, xs)
parseRbr x        = Just(False, x)

parseExpr :: String -> Maybe (AST, String)
parseExpr input = parseSum input

parseNum :: String -> Maybe (AST, String) 
parseNum input = 
    let (num, rest) = span isDigit input in 
    case num of 
      [] -> Nothing  
      xs -> Just (Num $ Sum.parseNum xs, rest)
  
  
parseOp :: String -> Maybe (Operator, String)
parseOp ('+':xs) = Just (Plus, xs)
parseOp ('*':xs) = Just (Mult, xs)
parseOp ('-':xs) = Just (Minus, xs)
parseOp ('/':xs) = Just (Div, xs)
parseOp _ = Nothing 

parseTerm :: String -> Maybe (AST, String)
parseTerm input = do
    (isL, rest) <- parseLbr input
    if isL then parseSum rest else parseNum input

parseMult :: String -> Maybe (AST, String)
parseMult input = do
    (num, rest) <- parseTerm input 
    case parseOp rest of 
      Just (op, rest') | op == Mult || op == Div -> do
        (r, rest'') <- parseMult rest'  
        return (BinOp op num r, rest'') 
      _ -> return (num, rest)
  

parseSum :: String -> Maybe (AST, String)
parseSum input = do 
  (l, rest) <- parseMult input 
  (isR, rest') <- parseRbr rest
  if (isR) then (return (l, rest')) else case parseOp rest' of 
    Just (op, rest'') | op == Plus || op == Minus -> do
      (r, rest''') <- parseSum rest''
      return (BinOp op l r, rest''') 
    _ -> return (l, rest)

evaluate :: String -> Maybe Int
evaluate input = do 
    (ast, rest) <- parseExpr input 
    return $ compute ast 

compute :: AST -> Int 
compute (Num x) = x 
compute (BinOp Plus x y) = compute x + compute y 
compute (BinOp Mult x y) = compute x * compute y 
compute (BinOp Minus x y) = compute x - compute y 
compute (BinOp Div x y) = compute x `div` compute y 

instance Show Operator where 
  show Plus = "+"
  show Mult = "*"
  show Minus = "-" 
  show Div = "/"

instance Show AST where
  show  = printf "\n%s" . go 0 
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $ 
        case t of 
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+1)
