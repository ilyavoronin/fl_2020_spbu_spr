module SimpleParsers where

import Combinators
import Control.Applicative
import Data.Char (isSpace)


string :: String -> Parser String String String
string [] = success ""
string (x:xs) = do 
  c <- symbol x
  s <- string xs
  return $ c:s


parseAny :: [Char] -> Parser String String Char
parseAny [a]    = symbol a
parseAny (x:xs) = (symbol x) <|> parseAny xs

parseAnyString :: [String] -> Parser String String String
parseAnyString [a]    = (string a)
parseAnyString (x:xs) = (string x) <|> parseAnyString xs

parseSpc :: Parser String String ()
parseSpc = do 
  many (fmap (\x->'a') parseComment <|> satisfy isSpace)
  return ()

parseComment :: Parser String String ()
parseComment = do
  string "D:"
  parseUntil ":D"
  return ()


parseUntil :: String -> Parser String String String
parseUntil s = (string s) <|> ((satisfy (\c -> True)) >> (parseUntil s)) 


parseOnlyEmpty :: Parser String String ()
parseOnlyEmpty = Parser 
    (\(InputStream input pos) -> if (input == "") then (Success (InputStream input pos) ()) else (Failure [ErrorMsg ["Can't parse whole string. Left:" ++ input] 0]))