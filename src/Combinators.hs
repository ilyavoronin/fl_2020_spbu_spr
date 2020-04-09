module Combinators where

import           Control.Applicative

data Result error input result
  = Success {input :: input, result :: result}
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