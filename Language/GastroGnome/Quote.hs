module Language.GastroGnome.Quote (gastrognome, IngredientExp(..), IngredientLit(..)) where

import System.IO.Unsafe (unsafePerformIO) 
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Text.Parsec.Pos

import Language.GastroGnome.CodeGen
import Language.GastroGnome.Parser
import Language.GastroGnome.Syntax (IngredientExp(..), IngredientLit(..))

gastrognome :: QuasiQuoter
gastrognome = QuasiQuoter (error "parse expression")
                          (error "parse pattern")
                          (error "parse type")
                          parseDecl

parseDecl :: String -> TH.Q [TH.Dec]
parseDecl input = do
  loc <- TH.location
  let fname = TH.loc_filename loc
  let (line,col) = TH.loc_start loc
  let sourcepos = newPos fname line col
  case parse parseProgram sourcepos input of
    Left err    -> unsafePerformIO $ fail $ show err
    Right decls -> makeGastroGnomeDecls decls
