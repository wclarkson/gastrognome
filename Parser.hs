module Parser where

import Data.Ratio
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Syntax

natural :: Parser Integer
natural = Token.natural (Token.makeTokenParser emptyDef)

fraction :: Parser (Ratio Integer)
fraction = do
  numerator <- natural
  char '/'
  denominator <- natural
  return (numerator % denominator)

parseCount :: Parser Quantity
parseCount =
  try (fraction >>= \f -> return $ Count f) <|>
  (natural >>= \c -> return $ Count (c % 1))

parseAmount :: Parser Quantity
parseAmount = do
  { Count amount <- parseCount
  ; spaces
  ; unit <- many1 $ noneOf " "
  ; return (Amount amount (Unit unit))
  }

parseQuantity :: Parser Quantity
parseQuantity = try parseAmount <|> parseCount

