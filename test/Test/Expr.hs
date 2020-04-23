module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (InputStream (..), Parser (..),
                                      Result (..), runParser,
                                      symbol, toStream, word)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), OpType (..), evaluate,
                                      parseExpr, parseNum, uberExpr, parseIdent)
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))
import           Combinators

testFailure = assertBool "" . isFailure

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
    evaluate "0||2+3||1" @?= Just 5
    evaluate "1&&0&&2+3" @?= Just 0
    evaluate "5&&2+3&&1+4*3" @?= Just 13
    evaluate "0||1&&2+3" @?= Just 5
    evaluate "2^2^3+5" @?= Just 261
    evaluate "5+1&&6==1+2*4-3||5" @?= Just 1
    evaluate "5+1<2^3&&6>=1+2*4-2||5" @?= Just 5
    evaluate "5+1<2^3&&(2+3)^4<=1+2*4-4||5" @?= Just 5
    evaluate "5+1<2^3&&6<=1+2*4-3||5" @?= Just 1

unit_parseNum :: Assertion
unit_parseNum = do
    runParser parseNum "7" @?= Success (toStream "" (Position 0 1)) (7)
    runParser parseNum "12+3" @?= Success (toStream "+3" (Position 0 2)) (12)
    runParser parseNum "007" @?= Success (toStream "" (Position 0 3)) (7)
    testFailure (runParser parseNum "+3")
    testFailure (runParser parseNum "a")


unit_parseIdent :: Assertion
unit_parseIdent = do
    runParser parseIdent "abc def" @?= Success (toStream "def" (Position 0 4)) "abc"
    runParser parseIdent "AbC dEf" @?= Success (toStream "dEf" (Position 0 4)) "AbC"
    runParser parseIdent "_123" @?= Success (toStream "" (Position 0 4)) "_123"
    runParser parseIdent "a_b_c d_e" @?= Success (toStream "d_e" (Position 0 6)) "a_b_c"
    runParser parseIdent "x_ " @?= Success (toStream "" (Position 0 3)) "x_"
    runParser parseIdent "abc123" @?= Success (toStream "" (Position 0 6)) "abc123"
    runParser parseIdent "_" @?= Success (toStream "" (Position 0 1)) "_"
    runParser parseIdent "abc*1" @?= Success (toStream "*1" (Position 0 3)) "abc"
    testFailure $ runParser parseIdent "123abc"
    testFailure $ runParser parseIdent "123"
    testFailure $ runParser parseIdent ""

erasePosition :: Result e i a -> Result e i a
erasePosition (Success str x) = Success (InputStream (stream str) (Position 0 0)) x
erasePosition x               = x



mult  = word "*" *> return Mult
sum'  = word "+" *> return Plus
minus = word "-" *> return Minus
div'  = word "/" *> return Div


expr1 :: Parser String String AST
expr1 =
  uberExpr [ (mult, Binary LeftAssoc)
           , (minus <|> div', Binary RightAssoc)
           , (sum', Binary NoAssoc)
           ]
           ((Num <$> parseNum) <|> symbol '(' *> expr1 <* symbol ')')
           BinOp
           UnaryOp

expr2 :: Parser String String AST
expr2 =
  uberExpr [(mult <|> div' <|> minus <|> sum', Binary LeftAssoc)]
           (Num <$> parseNum)
           BinOp
           UnaryOp

unit_expr1 :: Assertion
unit_expr1 = do
  runParser expr1 "13" @?= Success (toStream "" (Position 0 2)) (Num 13)
  runParser expr1 "(((1)))" @?= Success (toStream "" (Position 0 7)) (Num 1)
  runParser expr1 "1+2*3-4/5" @?= Success (toStream "" (Position 0 9)) (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (BinOp Minus (Num 3) (BinOp Div (Num 4) (Num 5))))
  runParser expr1 "1+2+3" @?= Success (toStream "+3" (Position 0 3)) (BinOp Plus (Num 1) (Num 2))
  runParser expr1 "1*2*3" @?= Success (toStream "" (Position 0 5)) (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr1 "1/2/3" @?= Success (toStream "" (Position 0 5)) (BinOp Div (Num 1) (BinOp Div (Num 2) (Num 3)))
  runParser expr1 "1-2-3" @?= Success (toStream "" (Position 0 5)) (BinOp Minus (Num 1) (BinOp Minus (Num 2) (Num 3)))
  runParser expr1 "1-2*3/4+5*6-7-8/9" @?= Success (toStream "" (Position 0 17)) (BinOp Mult (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (BinOp Plus (Num 4) (Num 5)))) (BinOp Minus (Num 6) (BinOp Minus (Num 7) (BinOp Div (Num 8) (Num 9)))))

unit_expr2 :: Assertion
unit_expr2 = do
  runParser expr2 "13" @?= Success (toStream "" (Position 0 2)) (Num 13)
  testFailure $ runParser expr2 "(((1)))"
  runParser expr2 "1+2*3-4/5" @?= Success (toStream "" (Position 0 9)) (BinOp Div (BinOp Minus (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5))
  runParser expr2 "1+2+3" @?= Success (toStream "" (Position 0 5)) (BinOp Plus (BinOp Plus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1*2*3" @?= Success (toStream "" (Position 0 5)) (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1/2/3" @?= Success (toStream "" (Position 0 5)) (BinOp Div (BinOp Div (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2-3" @?= Success (toStream "" (Position 0 5)) (BinOp Minus (BinOp Minus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2*3/4+5*6-7-8/9" @?= Success (toStream "" (Position 0 17)) (BinOp Div (BinOp Minus (BinOp Minus (BinOp Mult (BinOp Plus (BinOp Div (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5)) (Num 6)) (Num 7)) (Num 8)) (Num 9))
