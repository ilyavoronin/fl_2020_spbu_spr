module LLang where

import AST (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail', satisfy, success, symbol, parseAny, string, parseAnyString)
import Data.Char (isSpace, isDigit, digitToInt, isLetter)
import Control.Applicative

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseEvrExcSeq :: Parser String String LAst
parseEvrExcSeq = parseIf <|> parseWhile <|> parseRead <|> parseWrite <|> parseAssign

parseL :: Parser String String LAst
parseL = do
  lasts <- many parseEvrExcSeq
  return $ Seq lasts

parseSpc :: Parser String String ()
parseSpc = do 
  many (fmap (\x->'a') parseComment <|> satisfy isSpace)
  return ()


data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

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




parseIf :: Parser String String LAst
parseIf = do 
  (ifexpr, lastIf) <- do
    parseSpc
    string "if"
    parseSpc
    symbol '('
    parseSpc
    exprAst <- parseExpr
    parseSpc
    symbol ')'
    parseSpc
    symbol '{'
    parseSpc
    lastIf <- parseL
    symbol '}'
    parseSpc
    return (exprAst, lastIf)
  (elseExpr) <- (do
    parseSpc
    string "else"
    parseSpc
    symbol '{'
    parseSpc
    lastElse <- parseL
    parseSpc
    symbol '}'
    parseSpc
    return lastElse) <|> (success $ Seq [])
  return $ If ifexpr lastIf elseExpr

parseWhile :: Parser String String LAst
parseWhile = do
  parseSpc
  string "while"
  parseSpc
  symbol '('
  parseSpc
  condAst <- (parseExpr)
  parseSpc
  symbol ')'
  parseSpc
  symbol '{'
  parseSpc
  lastBody <- parseL
  parseSpc
  symbol '}'
  parseSpc
  return $ While condAst lastBody


parseAssign :: Parser String String LAst
parseAssign = do 
  parseSpc
  var <- parseIdent
  parseSpc
  symbol '='
  parseSpc
  expr <- parseExpr
  parseSpc
  symbol ';'
  parseSpc
  return $ Assign var expr


parseRead :: Parser String String LAst
parseRead = do
  parseSpc
  string "input"
  parseSpc
  symbol '('
  parseSpc
  var <- parseIdent
  parseSpc
  symbol ')'
  parseSpc
  symbol ';'
  parseSpc
  return $ Read var

parseWrite :: Parser String String LAst
parseWrite = do
  parseSpc
  string "print"
  parseSpc
  symbol '('
  parseSpc
  expr <- parseExpr
  parseSpc
  symbol ')'
  parseSpc
  symbol ';'
  parseSpc
  return $ Write expr

parseComment :: Parser String String ()
parseComment = do
  string "D:"
  parseUntil ":D"
  return ()


parseUntil :: String -> Parser String String String
parseUntil s = (string s) <|> ((satisfy (\c -> True)) >> (parseUntil s))