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