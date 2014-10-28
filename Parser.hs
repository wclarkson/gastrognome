module Parser where

import Data.Ratio
import Text.Parsec hiding (State, Parser, parse)
import Text.Parsec.Prim (ParsecT, runParserT)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec.Token
import Text.Parsec.String ()

import Syntax

type Parser a = ParsecT String () (State SourcePos) a

parse :: Parser a -> SourceName -> String -> Either ParseError a
parse p source input = runIndent source $ runParserT p () source input

mix = unlines [
    "BEAT with \"whisk\" until \"smooth\"",
    " Egg",
    " MIX",
    "  Flour",
    "  Sugar"
  ]

many2 p = liftM2 (:) p (many1 p)

parseNatural :: Parser Integer
parseNatural = do
  { d  <- oneOf "123456789"
  ; ds <- many digit
  ; return (read (d:ds))
  }

parseLabel :: Parser String
parseLabel = do
  { label <- many1 upper
  ; spaces
  ; return label
  }

parseItem :: Parser String
parseItem = do
  { item <- capWord
  ; spaces
  ; return item
  }

parseList :: Parser IngredientExp
parseList = do
  {
  ; b <- withBlock IngredientAction parseAction parseIngredientName
  ; spaces
  ; return b
  }

word :: Parser String
word = do
  { w <- many2 (noneOf " ")
  ; spaces
  ; return w
  }

lowerWord :: Parser String
lowerWord = do
  { w <- many2 lower
  ; spaces
  ; return w
  }

capWord :: Parser String
capWord = do
  { first <- upper
  ; rest <- many1 lower
  ; spaces
  ; return (first:rest)
  }

allCapWord :: Parser String
allCapWord = do
  { w <- many2 upper
  ; spaces
  ; return w
  }

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
  (try parseIngredientQuantity) <|>
  (try parseIngredientAction) <|>
  parseIngredientName

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
  ; adverbs <- many (try parseAdverb)
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
