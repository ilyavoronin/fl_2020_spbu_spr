module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..), Result (..))
import SimpleParsers
import Expr (parseIdent, parseExpr, evalExpr)
import Data.Char (isSpace, isDigit, digitToInt, isLetter)
import Control.Applicative
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

getLast :: Result a b LAst -> LAst
getLast (Success _ last) = last
getLast _                = Seq []


initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond lastIf lastElse) conf = do
  res <- (evalExpr (subst conf) cond)
  nconf <- case res of
    0         -> eval lastElse conf
    otherwise -> eval lastIf conf
  return nconf

eval (While cond last) conf = do
  res <- evalExpr (subst conf) cond
  nconf <- case res of
    0         -> Just conf
    otherwise -> do {
        newConf <- eval last conf;
        resConf <- eval (While cond last) newConf;
        return resConf;
    }
  return nconf

eval (Assign var exp) (Conf sub inp out) = do
  val <- evalExpr sub exp
  let newSub = Map.insert var val sub
  return $ Conf newSub inp out

eval (Read var) (Conf sub (x:xs) out) = do
  let newSub = Map.insert var x sub
  return $ Conf newSub xs out
eval (Read var) _                     = Nothing

eval (Write expr) (Conf sub inp out) = do
  val <- evalExpr sub expr
  return $ Conf sub inp (val:out)

eval (Seq []) conf = Just conf
eval (Seq (x:xs)) conf = do
  tconf <- eval x conf
  rconf <- eval (Seq xs) tconf
  return rconf


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

parsePrefix :: Parser String String LAst
parsePrefix = do
  lasts <- many parseEvrExcSeq
  return $ Seq lasts

parseL :: Parser String String LAst
parseL = do
  prog <- parsePrefix
  parseSpc
  parseOnlyEmpty
  return prog




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
    lastIf <- parsePrefix
    symbol '}'
    parseSpc
    return (exprAst, lastIf)
  (elseExpr) <- (do
    parseSpc
    string "else"
    parseSpc
    symbol '{'
    parseSpc
    lastElse <- parsePrefix
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
  lastBody <- parsePrefix
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
