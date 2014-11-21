module Language.GastroGnome.Quote (gastrognome, IngredientExp(..), IngredientLit(..)) where

import System.IO.Unsafe (unsafePerformIO) 
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Text.Parsec.Pos

import Language.GastroGnome.CodeGen
import Language.GastroGnome.Parser
import Language.GastroGnome.Syntax (IngredientExp(..), IngredientLit(..))

gastrognome :: QuasiQuoter
gastrognome = QuasiQuoter parseExp
                          (error "parse pattern")
                          (error "parse type")
                          parseDecl

parseAny :: String -> Parser a -> (a -> TH.Q b) -> TH.Q b
parseAny input p gen = do
  loc <- TH.location
  let fname = TH.loc_filename loc
  let (line,col) = TH.loc_start loc
  let sourcepos = newPos fname line col
  case parse p sourcepos input of
    Left err-> unsafePerformIO $ fail $ show err
    Right x -> gen x

parseExp :: String -> TH.Q TH.Exp
parseExp input = parseAny input parseIngredientExp makeGastroGnomeExp

parseDecl :: String -> TH.Q [TH.Dec]
parseDecl input = parseAny input parseProgram makeGastroGnomeDecls
