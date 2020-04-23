module Test.LLang where

import           AST
import           Combinators      (Result (..), runParser, toStream, InputStream (..))
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang 
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent, OpType (..))


accept input = InputStream "" (length input)

-- -- f x y = read z ; return (x + z * y)
-- -- g x = if (x) then return x else return x*13
-- -- {read x; read y; write (f x y); write (g x)}"

-- prog =
--   Program
--     [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
--     , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
--     ]
--     (
--       Seq
--         [ Read "x"
--         , Read "y"
--         , Write (FunctionCall "f" [Ident "x", Ident "y"])
--         , Write (FunctionCall "g" [Ident "x"])
--         ]
--     )


unit_lang1 :: Assertion
unit_lang1 = do
  let prog = "input(x); while(x > 0) {x = x - 1;} print(x);"
  let last = getLast (runParser parseL prog)
  eval last (initialConf [5]) @?= Just (Conf (Map.fromList[("x", 0)]) [] [0])

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]


unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing


unit_LLangTestIf :: Assertion
unit_LLangTestIf = do
  let input1 = "if ( \t \t  a==0 \n   )  { \n}" in
	runParser parseIf input1 @?= Success (accept input1) (If (BinOp Equal (Ident "a") (Num 0)) (Seq []) (Seq []))
  let input2 = "  if (    a==0 )  {  }      else { if  (b==5) {   }   }" in
	runParser parseIf input2 @?= Success (accept input2) (If (BinOp Equal (Ident "a") (Num 0))
	                                                                                     (Seq [])
	                                                                                     (Seq [If (BinOp Equal (Ident "b") (Num 5))
	                                                                                            (Seq [])
	                                                                                            (Seq [])    
	                                                                                         ]
	                                                                                     ))

unit_LLangTestWhile :: Assertion
unit_LLangTestWhile = do
  let input = "  while (b==0) {}" in
	runParser parseWhile input  @?= Success (accept input) (While (BinOp Equal (Ident "b") (Num 0)) (Seq []))


unit_LLangTestRead :: Assertion
unit_LLangTestRead = do
  let input = "input  ( asdfa''')  ;" in
	runParser parseRead input @?= Success (accept input) (Read "asdfa'''")

unit_LLangTestWrite :: Assertion
unit_LLangTestWrite = do
  let input = "print    (   b   ==0  )    ;" in
	runParser parseWrite input @?= Success (accept input) (Write (BinOp Equal (Ident "b") (Num 0)))

unit_LLangTestAssign :: Assertion
unit_LLangTestAssign = do
  let input = "AsD2f'''    =   b  ==   0   ;   " in
	runParser parseAssign input @?= Success (accept input) (Assign "AsD2f'''" (BinOp Equal (Ident "b") (Num 0)))


unit_LLangTestAll :: Assertion
unit_LLangTestAll = do
  let input1 = "input(n);\
                   \input(elem');\
                   \l = 0;\
                   \r = n; \
                   \while ( (r - l)  >   1 ) {\
                   \    m = (r - l) /2;  \
                   \    if (m > elem' ) {\
                    \      r = m;    \
                    \}\
                    \ else {\
                    \      l = m;\
                    \}\
                   \}\
                   \print(l);\
                   \" in
	runParser parseL input1 @?= Success (accept input1) (Seq [
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
  let input2 = "input(c); x = (  1  ) + -  2;" in
	runParser parseL input2 @?= Success (accept input2) (Seq [Read "c", Assign "x" (BinOp Plus (Num 1) (UnaryOp Minus (Num 2)))])



unit_LLangTestComment :: Assertion
unit_LLangTestComment = do
  let input = "D:print(dfdfd) :D print(5); \n" in
	runParser parseL input  @?= Success (accept input) (Seq [Write (Num 5)])




unit_LLangTestDog :: Assertion
unit_LLangTestDog = do
  let input = "D:\
\:-::/::------:::::::::::::::::osso+o++++++++++++++++////:::::/::::::::::::::::://////////:::::::-:::\
\:::::::::::::::--::::-:::::::::::::::::://////////////////////:::::::::::::--::-:-------::::::::::::\
\////////////////////////++////////:::-::::::::::::::::::::::::::///:::/::::/:::::/:::::///::::::::::\
\//////:Dinput(n);D:::::::::::---...----:::-::::-:://///////////+///////////////////:::::::::::::::::\
\///////////////////////////:--..```...---------.....-:::::/:Dinput(elem');D:////////////////////////\
\/+//:Dl=0;D://///////////::-.`````...----------...........--:///////////////////////////////////////\
\////+++++++++++//+///::--..`````...---::::-------......````.-:////////////////////++++:Dr=n;D:++++++\
\+++++++:DwhileD:+/////:---...````...---::::::::-------....```.--:/+++++////:D((r-l)>1){D:///////////\
\+++++++++++++////::-----....``...---:::::::::::::----.......-::/+++++///////////////////////////////\
\+++++++++++++///::-------..```...-:::---::::/:::::::::-----::///////////////////////////////////////\
\+++++++++++++/:--...-:::-.`.....---------::::::::::::://::///++/////////////////////////////////////\
\sssssssssso+/:-....-:://:......---::-...----::::--::/++o+///+oo+//////////+++//////////////////////+\
\+///////////:-.---::////:....-:///::-------::--::--:/+oooooo+ooo++///////////////////++++++++++++++/\
\+++++++++//::--:/++++++/:-.-:/+/:://+/::::::::://+o+++oossysoossso+/++//+++++++:Dm=(r-l)/2;D://////\
\++++++++////:://+oo++o+/:..:///:/ossso+::::/:://ohyyysooosyoooosys+/+ooo+++++++++///+++++++++///////\
\++++++//////++++ossooo+/--:++::+shhhyy+/-.--:-:/shdmdhs++syooo++ooo+++++++++++++++++++++++++++//////\
\++++////++ooooossyysso+/::/+/////++++++-``````.:syyyhhdsoys+oooooooo++++++++++++++++++++++++++/////+\
\++/////++ooossyhddyyss+:::+/::--..-:--.`      `.-/osssyy+so+++ssssssso++++++++++++++++++++++++ooosso\
\++////++ossyyyyhdhhhyo////:--.````````        ````-://++/+/++ossssssso++++++++++++++oossssssoooo++//\
\ssooo+ossyyyssssyyhyooo+:..``            `````..```...-:/++/+osssssoo+++++:Dif (m >elem'){D:++++++++\
\so++/o+oosoo+o+++ooo+oo/-``          ```........----.``.-:soo+oosssoooo++++++///+++++++++ooo++++++++\
\o+++++++++++++++++oo+/:.``       `````...::::::/+//:--....+sssosssoo+++/++++++++++++++++++++++++oo++\
\o++++++++++++++++oo++-.```      `````..../osssyyyo//:--...:yyyyyssoo+++:Dr = m;}D:+//+ooooo++++ooosy\
\++++++++++++++++++oo/-..`````  `..-.-:::::shhdddyo++/::---/shyyyyoo+++///+//+++++++++oooosyyssoo+/:-\
\++++++++++++++++++ooo/:-.``````.-///oo+++//syhysooo++//::/osddhhyso+++/++//+++++oosyssoo+/--........\
\++++++++++++++/++++oo+/:....`..-:+++/+++++osyssssssoo++:/+syhhhhys++++++oossssoo+:-...............-.\
\++++oossoooo++////++oo+/--...-::/+o////::://///+++oooo+/++syyssssooosso+/:-...................--....\
\osyssoo+/////////://+oss+/::-:/++++//:::://+/////+sooo++ooyys+/+++/:-..................---..........\
\/++++++++++++//::::://+ssoo////++++/:::://+++++/+osooossyyyso++//+/:-.....................-...------\
\o++ooo++++++//::::::::/++ossssso+++/::::/+ooo//+ossyyhhhyysso+++////:----...............---------:::\
\+++++++++++///::--::://///+ssssssso+//:/oosssooyyhhhhhyyysoooooo+++++/-............-------:::/:::--.\
\ooo++o++++++//:---:::///://+++++ossoo+oossyyyyyhhhhhyyyyssooo++oooo++/:.....--------::://::---......\
\+++++++++++++/:------:::////////+ooo++ooossyyyyhhyyyysssssooo+ooooo++//:-----:::::::---...........--\
\o+++:Delse{l=m;}}D::/:/:////+++/+osoosyyyyysssssssssssoooooooo+++/::::::--....----------------------\
\oooo++/::--..---...-----:::::////++++oosooosssssssoosssosssoooooooo+++/:--..............----.--:::::\
\--...........---..-..------::::://////++++ooooooooooosssosooooooooooo/:---...-..---------::::::--...\
\.``.........-----------------:::::::::://+++//++++oooooooosssoossoo+o+:-----------::::::::--........\
\:..........----.---------.-----------:::////////+++++++oooossssoooooo+/:::::::::::---.......---.....\
\:-.--............-------..----------::::://:::://////+++oossssssossso++/++/::---......-----.-.......\
\-:-..-............-----..---::---:::///::/:::::://////++oossssssssssso+/::-----..-----..............\
\.-:---............-----.--::/:--:::/+++/:::::::::////++oosssssssyyso+/::-----------..............-:/\
\:::-............--::::-----::::-://++oo//////::/:/++++oossyyyyyyyyo+/:::------:Dprint(l);D:.-:/+ooo+\
\:::-....`..--:::::--.......--:::://ooso++/////////+ooosssydddhhyso+//:::::::----.....---:/+ooo+:--..\
\::::---::::--..........-.``..--::/+ooso+++///+/++++oossyhddhhyso+///:::-----------://+ooo+::-.....--\
\:////-..............---`````..--:/+osyso+//////+++ooossyhhyysoo+///:::-------:/+ooso+:-......--:/+oo\
\///::.....-.-.....----.``````..--/+shdhso///////+++++ossyysso++//::::::::/+osso+/--.....--:/+ooooooo\
\.---------........-----```....---/oshdhyo/:::::://///+osssoo++//////+osyso+/:-.....--:/+oooooooooooo\
\.-----...----....--:///------::/+ooyysssso/-----:::://ssooo++++oosyyso/:---.----:/+ooooooooooooooooo\
\----:/--...---::::::---::/+++oooooooooooooo:-----::/+ssoooosyyyso/::-------:/++oooooooooooooooooossy\
\////:::-:/:::--------::::::::///++++++++++++/:/:/+ooysyyhyys+/::-----::/+ooosoooooooooooooooosyyysoo\
\-------/-....--.---::-::::::://////////++/++++osyhhhhyso/:::::::::/+oooooooooooooooooooossyyysoooooo\
\:D" in
	runParser parseL input @?= Success (accept input)  (Seq [
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



unit_LLangProgramTest :: Assertion
unit_LLangProgramTest = do
  let input = "  !sum (n m) {  return! n + m; }  !first ( n m ) { n = m; return! n; } while ( ?sum (1 2) + ?first ( 2 3 ) ) {print(555);}"
      function1 =  ( Function "sum" ["n", "m"] (Seq [Return (BinOp Plus (Ident "n") (Ident "m"))]))
      function2 =  ( Function "first" ["n", "m"] (Seq [Assign "n" (Ident "m"), Return (Ident "n")])) in
   runParser parseProg input @?= Success (accept input) ( Program [function1, function2] (Seq [While 
               (BinOp Plus (FunctionCall "sum" [Num 1, Num 2]) (FunctionCall "first" [Num 2, Num 3])) 
               (Seq [Write (Num 555)])]))

unit_LLangFunctionTest :: Assertion
unit_LLangFunctionTest = do
  let input = "!sum (n m) {  return! n + m; }" in
   runParser parseDef input @?= Success (accept input) ( Function "sum" ["n", "m"] (Seq [Return (BinOp Plus (Ident "n") (Ident "m"))]))
