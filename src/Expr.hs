module Expr where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..), Result (..))
import           SimpleParsers
import           Data.Char   (digitToInt, isDigit, isLetter)
import           Control.Applicative
import qualified Data.Map as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = error "Not implemented"

uberExpr :: [(Parser String String op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser String String ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser String String ast
uberExpr ((opar, typ):xs) epar fast bast = let 
  combpar = uberExpr xs epar fast bast
  parseMany = (,) <$> combpar <*> (many ((,) <$> opar <*> combpar)) in
  case typ of
    Binary assoc -> case assoc of
      LeftAssoc -> do
        (first, arr) <- parseMany
        return $ foldl (\acc (op, rast) -> fast op acc rast) first arr
      RightAssoc  -> do
        (first, arr') <- parseMany
        let (end, arr) = foldl (\(acc, narr) (op, ast) -> (ast, (acc,op):narr)) (first, []) arr'
        return $ foldr (\(rast, op) acc -> fast op rast acc) end (reverse arr)
      NoAssoc    -> (do
        ast1 <- combpar
        op   <- opar
        ast2 <- combpar
        return $ fast op ast1 ast2) <|> combpar
    Unary -> (do
      op <- opar
      ast <- combpar
      return  $ bast op ast) <|> combpar

uberExpr [] epar _ _ = epar


toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "/"  = success Div
toOperator "^"  = success Pow
toOperator "||" = success Or
toOperator "&&" = success And
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator "<=" = success Le
toOperator "<"  = success Lt
toOperator ">=" = success Ge
toOperator ">"  = success Gt
toOperator "!"  = success Not
toOperator _    = fail' "Failed toOperator"

parseOp :: String -> Parser String String Operator
parseOp s = parseSpc *> (string s >>= toOperator) <* parseSpc


mult'   = parseOp "*"
sum'    = parseOp "+"
minus'  = parseOp "-"
div'    = parseOp "/"
pow'    = parseOp "^"
or'     = parseOp "||"
and'    = parseOp "&&"
equal'  = parseOp "=="
nequal' = parseOp "/="
le'     = parseOp "<="
lt'     = parseOp "<"
ge'     = parseOp ">="
gt'     = parseOp ">"
not'    = parseOp "!"



-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки

operatorsParsers = [(or', Binary LeftAssoc),
                    (and', Binary LeftAssoc),
                    (not', Unary),
                    (equal' <|> nequal' <|> le' <|> lt' <|> ge' <|> gt', Binary NoAssoc),
                    (sum' <|> minus', Binary LeftAssoc),
                    (mult' <|> div', Binary LeftAssoc),
                    (minus', Unary),
                    (pow', Binary RightAssoc)]

parseExpr :: Parser String String AST
parseExpr = uberExpr
            operatorsParsers
            (Num <$> parseNum <|> Ident <$> parseIdent <|> parseSpc *> symbol '(' *> parseSpc *> parseExpr <* parseSpc <* symbol ')' <* parseSpc)
            BinOp
            UnaryOp


isDigitW0 d = (d /= '0') && (isDigit d)

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 `fmap` go
  where
    go :: Parser String String String
    go = parseSpc *> some (satisfy isDigit) <* parseSpc

isLetterOrUnderscore = (satisfy isLetter) <|> (symbol '_')

parseIdent :: Parser String String String
parseIdent = do
  parseSpc
  t1 <- some isLetterOrUnderscore
  t2 <- many (isLetterOrUnderscore <|> (satisfy isDigit))
  t3 <- many (symbol '\'')
  parseSpc
  return $ t1 ++ t2 ++ t3

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing


boolToInt True = 1
boolToInt _    = 0

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = (compute x) ^ (compute y)
compute (BinOp Or  x y)   = case (compute x) of
                               p | p == 0 -> compute y
                               p          -> p
compute (BinOp And x y)   = case (compute x) of
                               p | p /= 0 -> compute y
                               p          -> p
compute (BinOp Equal x y) = boolToInt $ (compute x) == (compute y)
compute (BinOp Nequal x y) = boolToInt $ (compute x) /= (compute y)
compute (BinOp Le     x y) = boolToInt $ (compute x) <= (compute y)
compute (BinOp Lt     x y) = boolToInt $ (compute x) < (compute y)
compute (BinOp Ge     x y) = boolToInt $ (compute x) >= (compute y)
compute (BinOp Gt     x y) = boolToInt $ (compute x) > (compute y)
compute (UnaryOp Not    x) = if (compute x == 0) then 1 else 0
compute (UnaryOp Minus  x) = -(compute x)