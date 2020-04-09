module SimpleParsers where

import Combinators
import Control.Applicative
import Data.Char (isSpace)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

string :: String -> Parser String String String
string [] = success ""
string (x:xs) = do 
  c <- symbol x
  s <- string xs
  return $ c:s

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    _            -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure


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
    (\input -> if (input == "") then (Success "" ()) else (Failure ("Can't parse whole string. Left:" ++ input)))