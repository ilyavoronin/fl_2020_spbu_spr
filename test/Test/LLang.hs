module Test.LLang where

import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import           LLang 
import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser, string, symbol)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent, OpType (..))


unit_LLangTestIf :: Assertion
unit_LLangTestIf = do
	runParser parseIf "  if ( \t \t  a==0 \n   )  { \n}" @?= Success "" (If (BinOp Equal (Ident "a") (Num 0)) (Seq []) (Seq []))
	runParser parseIf "  if (    a==0 )  {  }      else { if  (b==5) {   }   }" @?= Success "" (If (BinOp Equal (Ident "a") (Num 0))
	                                                                                     (Seq [])
	                                                                                     (Seq [If (BinOp Equal (Ident "b") (Num 5))
	                                                                                            (Seq [])
	                                                                                            (Seq [])    
	                                                                                         ]
	                                                                                     ))

unit_LLangTestWhile :: Assertion
unit_LLangTestWhile = do
	runParser parseWhile "  while (b==0) {}"  @?= Success "" (While (BinOp Equal (Ident "b") (Num 0)) (Seq []))


unit_LLangTestRead :: Assertion
unit_LLangTestRead = do
	runParser parseRead "input  ( asdfa''')  ;" @?= Success "" (Read "asdfa'''")

unit_LLangTestWrite :: Assertion
unit_LLangTestWrite = do
	runParser parseWrite "print    (   b==0  )    ;" @?= Success "" (Write (BinOp Equal (Ident "b") (Num 0)))

unit_LLangTestAssign :: Assertion
unit_LLangTestAssign = do
	runParser parseAssign "AsD2f'''    =   b==0   ;   " @?= Success "" (Assign "AsD2f'''" (BinOp Equal (Ident "b") (Num 0)))


unit_LLangTestAll :: Assertion
unit_LLangTestAll = do
	runParser parseL "input(n);\
	                 \input(elem');\
	                 \l = 0;\
	                 \r = n; \
	                 \while ( r-l>1 ) {\
	                 \    m = (r-l)/2;  \
	                 \    if (m>elem') {\
	                 	\      r = m;    \
	                 	\}\
	                 	\ else {\
	                 	\      l = m;\
	                 	\}\
	                 \}\
	                 \print(l);\
	                 \"
	                   @?= Success "" (Seq [
	                       Read "n",
	                       Read "elem'",
	                       Assign "l" (Num 0),
	                       Assign "r" (Ident "n"),
	                       While (BinOp Gt (BinOp Minus (Ident "r") (Ident "l")) (Num 1)) (Seq [
	                       	    Assign "m" (BinOp Div (BinOp Minus (Ident "r") (Ident "l")) (Num 2)),
	                       	    If (BinOp Gt (Ident "m") (Ident "elem'")) (Seq [
	                       	        Assign "r" (Ident "m")
	                       	    ])
	                       	    (Seq [
                                    Assign "l" (Ident "m")
	                       	    ])
	                       	]),
	                       Write (Ident "l")
	                   ])