module LEval where

import LLang
import Combinators
import AST
import Control.Applicative
import Control.Monad
import Expr
import qualified Data.Map            as Map

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function
type Subst = Map.Map String Int


initialConf :: [Int] -> Defs -> Configuration
initialConf input func = Conf Map.empty input [] func

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program functions main) input = let 
    nameToFunc = foldl (\acc f -> Map.insert (name f) f acc) Map.empty functions in
    eval main (initialConf input nameToFunc)



parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg progText input = do
	prog <- case (runParser parseProg progText) of
		Success _ prog -> Just prog
		otherwise      -> Nothing
	evalProg prog input


evalArgs :: Configuration -> [AST] -> Maybe ([Int], Configuration)
evalArgs conf [] = Just ([], conf)
evalArgs conf (x:xs) = do 
	(t, conf1) <- (evalExpr conf x)
	(re, conf2) <- (evalArgs conf1 xs)
	return ((t:re), conf2)

evalFunctionCall :: Configuration -> [AST] -> Function -> Maybe (Int, Configuration)
evalFunctionCall conf@(Conf subst _ _ defs) exprs (Function _ args body ret) = do
	(vals, conf1) <- (evalArgs conf exprs)
	let nsubst = foldl (\acc (arg, val) -> Map.insert arg val acc) (Map.empty) (zip args vals)
	nconf <- eval body (Conf nsubst (input conf1) (output conf1) defs)
	(t, conf2) <- evalExpr nconf ret
	return $ (t, Conf subst (input conf2) (output conf2) defs)


evalExpr :: Configuration -> AST -> Maybe (Int, Configuration)
evalExpr conf (BinOp op ast1 ast2) =  do
  (res1, conf1) <- (evalExpr conf ast1)
  (res2, conf2) <- (evalExpr conf1 ast2)
  return $ (computeBinOp op res1 res2, conf2)
evalExpr conf (UnaryOp op ast) = do
  (res, conf1) <- (evalExpr conf ast)
  return $ (computeUnaryOp op res, conf1)
evalExpr conf (Num n) = Just (n, conf)
evalExpr conf (Ident s) = do
  val <- Map.lookup s (subst conf)
  return (val, conf)
evalExpr conf (FunctionCall name exprs) = do
	f <- Map.lookup name (defs conf)
	guard (length exprs == length (args f))
	evalFunctionCall conf exprs f


eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond lastIf lastElse) conf = do
  (res, conf1) <- evalExpr conf cond
  nconf <- case res of
    0         -> eval lastElse conf1
    otherwise -> eval lastIf conf1
  return nconf

eval (While cond last) conf = do
  (res, conf1) <- evalExpr conf cond
  nconf <- case res of
    0         -> Just conf1
    otherwise -> do {
        newConf <- eval last conf1;
        resConf <- eval (While cond last) newConf;
        return resConf;
    }
  return nconf

eval (Assign var exp) conf@(Conf sub inp out def) = do
  (val, conf1) <- evalExpr conf exp
  let newSub = Map.insert var val sub
  return $ Conf newSub (input conf1) (output conf1) def

eval (Read var) (Conf sub (x:xs) out defs) = do
  let newSub = Map.insert var x sub
  return $ Conf newSub xs out defs
eval (Read var) _                     = Nothing

eval (Write expr) conf@(Conf sub inp out defs) = do
  (val, conf1) <- evalExpr conf expr
  return $ Conf sub (input conf1) (val:(output conf1)) defs

eval (Seq []) conf = Just conf
eval (Seq (x:xs)) conf = do
  tconf <- eval x conf
  rconf <- eval (Seq xs) tconf
  return rconf