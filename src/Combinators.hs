module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f p = Parser runParser' where
    runParser' input = case (runParser p input) of
      Failure error         -> Failure error
      Success input' result -> Success input' (f result)

instance Applicative (Parser error input) where
  pure result = Parser (\input -> Success input result)
  (<*>) fp p = 
    Parser runParser' where
      runParser' input =  case (runParser fp input) of
        Failure error          -> Failure error
        Success input' fresult -> case (runParser p input') of
                                   Failure error          -> Failure error
                                   Success input'' result -> Success input'' (fresult result)


instance Monad (Parser error input) where
  return = pure

  (>>=) p f = 
    Parser runParser' where
      runParser' input = case (runParser p input) of
                           Failure error         -> Failure error
                           Success input' result -> (runParser (f result) input')


instance Monoid error => Alternative (Parser error input) where
  empty = Parser (\input -> Failure mempty)

  (<|>) p1 p2 = 
    Parser runParser' where
      runParser' input = case (runParser p1 input) of
                           Failure error -> runParser p2 input
                           ot            -> ot

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (some (sep *> elem))


-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

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