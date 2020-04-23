module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..), Result (..), symbol, success)
import SimpleParsers
import Expr (parseIdent, parseExpr, evalExpr)
import Data.Char (isSpace, isDigit, digitToInt, isLetter)
import Control.Applicative
import qualified Data.Map as Map
import           Data.List   (intercalate)
import           Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }
                deriving (Eq)

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }
              deriving (Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

getLast :: Result a b LAst -> LAst
getLast (Success _ last) = last
getLast _                = Seq []


parseDef :: Parser String String Function
parseDef = do
  parseSpc
  string "!"
  name <- parseIdent
  parseSpc
  string "("
  args <- many (parseSpc *> parseIdent <* parseSpc)
  string ")"
  parseSpc
  string "{"
  last <- parseL
  string "}"
  parseSpc
  return $ Function name args last

parseProg :: Parser String String Program
parseProg = do
  parseSpc
  functions <- many (parseSpc *> parseDef <* parseSpc)
  main <- parseL
  parseSpc
  parseOnlyEmpty
  return $ Program functions main


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
parseEvrExcSeq = parseIf <|> parseWhile <|> parseRead <|> parseWrite <|> parseAssign <|> parseReturn


parseL :: Parser String String LAst
parseL = do
  lasts <- many parseEvrExcSeq
  return $ Seq lasts




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

parseReturn :: Parser String String LAst
parseReturn = do
  parseSpc
  string "return!"
  parseSpc
  expr <- parseExpr
  parseSpc
  string ";"
  parseSpc
  return $ Return expr

instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)


instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id