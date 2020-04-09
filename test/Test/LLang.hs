module Test.LLang where

import           AST              (AST (..), Operator (..))
import qualified Data.Map         as Map
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           LLang 
import           Combinators         (Parser (..), Result (..), runParser)
import           Control.Applicative ((<|>))
import           Expr                (Associativity (..), evaluate, parseExpr,
                                      parseNum, parseOp, toOperator, uberExpr, parseIdent, OpType (..))

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

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
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
	runParser parseWrite "print    (   b   ==0  )    ;" @?= Success "" (Write (BinOp Equal (Ident "b") (Num 0)))

unit_LLangTestAssign :: Assertion
unit_LLangTestAssign = do
	runParser parseAssign "AsD2f'''    =   b  ==   0   ;   " @?= Success "" (Assign "AsD2f'''" (BinOp Equal (Ident "b") (Num 0)))


unit_LLangTestAll :: Assertion
unit_LLangTestAll = do
	runParser parseL "input(n);\
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
	runParser parseL "input(c); x = (  1  ) + -  2;" @?= Success "" (Seq [Read "c", Assign "x" (BinOp Plus (Num 1) (UnaryOp Minus (Num 2)))])



unit_LLangTestComment :: Assertion
unit_LLangTestComment = do
	runParser parseL "D:print(dfdfd) :D print(5); asdf" @?= Success "asdf" (Seq [Write (Num 5)])




unit_LLangTestDog :: Assertion
unit_LLangTestDog = do
	runParser parseL "D:\
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
\:D" @?= Success "" (Seq [
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
