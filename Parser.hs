module Parser where

import Data.Ratio
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.ParserCombinators.Parsec hiding (State, Parser, parse)
import Text.Parsec.Prim (ParsecT, runParserT)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec.String ()

import Syntax

type Parser a = ParsecT String () (State SourcePos) a

parse :: Parser a -> SourceName -> String -> Either ParseError a
parse p source input = runIndent source $ runParserT p () source input

parseNatural :: Parser Integer
parseNatural = do
  { d  <- oneOf "123456789"
  ; ds <- many digit
  ; return (read (d:ds))
  }

parseLabel :: Parser String
parseLabel = do
  { label <- many1 upper
  ; char ':'
  ; spaces
  ; return label
  }

parseItem :: Parser String
parseItem = do
  { item <- many1 lower
  ; spaces
  ; return item
  }

parseList :: Parser (String, [String])
parseList = do
  {
  ; b <- withBlock (\x y -> (x, y)) parseLabel parseItem
  ; spaces
  ; return b
  }

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
  numerator <- parseNatural
  char '/'
  denominator <- parseNatural
  return (numerator % denominator)

parseIngredientQuantity :: Parser IngredientExp
parseIngredientQuantity = do
  { quantity <- parseQuantity
  ; spaces
  ; ingredient <- parseIngredientLit
  ; return (IngredientQuantity quantity ingredient)
  }

parseIngredientName :: Parser IngredientExp
parseIngredientName = parseIngredientLit >>= return . IngredientName

parseIngredientAction :: Parser IngredientExp
parseIngredientAction = withBlock IngredientAction parseAction parseIngredientExp

parseIngredientExp :: Parser IngredientExp
parseIngredientExp =
  (try parseIngredientQuantity) {-<|>
  (try parseIngredientAction) <|>
  parseIngredientName -}

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

parseCountFraction :: Parser Quantity
parseCountFraction = try (fraction >>= \f -> return $ Count f)

parseCountNatural :: Parser Quantity
parseCountNatural = (parseNatural >>= \c -> return $ Count (c % 1))

parseCount :: Parser Quantity
parseCount = parseCountFraction <|> parseCountNatural

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
