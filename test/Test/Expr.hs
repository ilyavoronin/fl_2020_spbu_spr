module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr)
import           Test.Tasty.HUnit    (Assertion, (@?=))

isFailure (Failure _) = True
isFailure  _          = False

unit_evaluate :: Assertion
unit_evaluate = do
    evaluate "1" @?= Just 1
    evaluate "1+2" @?= Just (1+2)
    evaluate "2+4+8" @?= Just (2+4+8)
    evaluate "11+22" @?= Just (11+22)
    evaluate "13+42+777" @?= Just (13+42+777)
    evaluate "31+24+777" @?= Just (31+24+777)
    evaluate "1+2*3+4" @?= Just (1+2*3+4)
    evaluate "12+23*34+456" @?= Just (12+23*34+456)
    evaluate "1-2*3+4" @?= Just (1-2*3+4)
    evaluate "1-2-3" @?= Just (1-2-3)
    evaluate "4/2-2" @?= Just (4 `div` 2 - 2)
    evaluate "(1+2)*(3+4)" @?= Just ((1+2)*(3+4))
    evaluate "12+(23*(34)+456)" @?= Just (12+(23*(34)+456))
    evaluate "((1-(2*3))+4)" @?= Just ((1-(2*3))+4)
    evaluate "1-2+3-4" @?= Just (1-2+3-4)
    evaluate "6/2*3" @?= Just (6 `div` 2 * 3)

unit_parseNum :: Assertion
unit_parseNum = do
    runParser parseNum "7" @?= Success "" 7
    runParser parseNum "12+3" @?= Success "+3" 12
    runParser parseNum "007" @?= Success "" 7
    isFailure (runParser parseNum "+3") @?= True
    isFailure (runParser parseNum "a") @?= True

unit_parseOp :: Assertion
unit_parseOp = do
    runParser parseOp "+1" @?= Success "1" Plus
    runParser parseOp "**" @?= Success "*" Mult
    runParser parseOp "-2" @?= Success "2" Minus
    runParser parseOp "/1" @?= Success "1" Div
    isFailure (runParser parseOp "12") @?= True

unit_parseExpr :: Assertion
unit_parseExpr = do
    runParser parseExpr "1*2*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseExpr "123"     @?= Success "" (Num 123)
    runParser parseExpr "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseExpr "1+2*3+4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4)) 



mult  = symbol '*' >>= toOperator
sum'  = symbol '+' >>= toOperator
minus = symbol '-' >>= toOperator
div'  = symbol '/' >>= toOperator

expr1 :: Parser String String AST
expr1 =
  uberExpr [ (mult, LeftAssoc)
           , (minus <|> div', RightAssoc)
           , (sum', NoAssoc)
           ]
           (Num <$> parseNum <|> symbol '(' *> expr1 <* symbol ')')
           BinOp

expr2 :: Parser String String AST
expr2 =
  uberExpr [(mult <|> div' <|> minus <|> sum', LeftAssoc)]
           (Num <$> parseNum)
           BinOp

expr3 :: Parser String String AST
expr3 = 
  uberExpr [(mult, RightAssoc), (sum', LeftAssoc), (div', NoAssoc)] (Num <$> parseNum) BinOp

unit_expr1 :: Assertion
unit_expr1 = do
  runParser expr1 "13" @?= Success "" (Num 13)
  runParser expr1 "(((1)))" @?= Success "" (Num 1)
  runParser expr1 "1+2*3-4/5" @?= Success "" (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (BinOp Minus (Num 3) (BinOp Div (Num 4) (Num 5))))
  runParser expr1 "1+2+3" @?= Success "+3" (BinOp Plus (Num 1) (Num 2))
  runParser expr1 "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr1 "1/2/3" @?= Success "" (BinOp Div (Num 1) (BinOp Div (Num 2) (Num 3)))
  runParser expr1 "1-2-3" @?= Success "" (BinOp Minus (Num 1) (BinOp Minus (Num 2) (Num 3)))
  runParser expr1 "1-2*3/4+5*6-7-8/9" @?= Success "" (BinOp Mult (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (BinOp Plus (Num 4) (Num 5)))) (BinOp Minus (Num 6) (BinOp Minus (Num 7) (BinOp Div (Num 8) (Num 9)))))

unit_expr2 :: Assertion
unit_expr2 = do
  runParser expr2 "13" @?= Success "" (Num 13)
  runParser expr2 "(((1)))" @?= Failure "Predicate failed"
  runParser expr2 "1+2*3-4/5" @?= Success "" (BinOp Div (BinOp Minus (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5))
  runParser expr2 "1+2+3" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1/2/3" @?= Success "" (BinOp Div (BinOp Div (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2-3" @?= Success "" (BinOp Minus (BinOp Minus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2*3/4+5*6-7-8/9" @?= Success "" (BinOp Div (BinOp Minus (BinOp Minus (BinOp Mult (BinOp Plus (BinOp Div (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5)) (Num 6)) (Num 7)) (Num 8)) (Num 9))


unit_expr3 :: Assertion
unit_expr3 = do
  runParser expr3 "13" @?= Success "" (Num 13)