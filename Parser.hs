module Parser where

import Data.Ratio
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Syntax

natural :: Parser Integer
natural = Token.natural (Token.makeTokenParser emptyDef)

word :: Parser String
word = many1 (noneOf " ")

lowerWord :: Parser String
lowerWord = many1 lower

capWord :: Parser String
capWord = do
  { first <- upper
  ; rest <- many1 lower
  ; return (first:rest)
  }

allCapWord :: Parser String
allCapWord = many1 upper

quoted :: Parser String -> Parser String
quoted p = do
  { char '"'
  ; w <- p
  ; char  '"'
  ; return w
  }

fraction :: Parser (Ratio Integer)
fraction = do
  numerator <- natural
  char '/'
  denominator <- natural
  return (numerator % denominator)

parseIngredientLit :: Parser IngredientLit
parseIngredientLit = capWord >>= return . IngredientLit

parseDefaultQuantityDecl :: Parser DefaultQuantityDecl
parseDefaultQuantityDecl = do
  { quantity   <- parseQuantity
  ; spaces
  ; ingredient <- parseIngredientLit
  ; return (DefaultQuantityDecl quantity ingredient)
  }

parseAdverb :: Parser (String, String)
parseAdverb = do
  { conn <- word
  ; spaces
  ; adv <- quoted (many1 (noneOf "\""))
  ; spaces
  ; return (conn, adv)
  }

parseAction :: Parser Action
parseAction = do
  { verb <- allCapWord
  ; spaces
  ; adverbs <- many parseAdverb
  ; return (Action verb adverbs)
  }

parseActionDecl :: Parser ActionDecl
parseActionDecl = do
  { newAction <- allCapWord
  ; spaces
  ; char '='
  ; spaces
  ; exAction <- parseAction
  ; return (ActionDecl newAction exAction)
  }


parseCount :: Parser Quantity
parseCount =
  try (fraction >>= \f -> return $ Count f) <|>
  (natural >>= \c -> return $ Count (c % 1))

parseAmount :: Parser Quantity
parseAmount = do
  { Count amount <- parseCount
  ; spaces
  ; unit <- parseUnit
  ; return (Amount amount unit)
  }

parseQuantity :: Parser Quantity
parseQuantity = try parseAmount <|> parseCount

parseUnit :: Parser Unit
parseUnit = lowerWord >>= return . Unit

parseUnitDecl :: Parser UnitDecl
parseUnitDecl = do
  {
  ; Count leftQ   <- parseCount
  ; spaces
  ; newUnit <- lowerWord
  ; spaces
  ; char '='
  ; spaces
  ; Count rightQ  <- parseCount
  ; exUnit  <- parseUnit
  ; return (UnitDecl leftQ newUnit rightQ exUnit)
  }
