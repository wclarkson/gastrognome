module Syntax where
import Data.Ratio

data IngredientDecl = IngredientDecl String IngredientExp deriving Show
data IngredientExp  = IngredientExp Quantity IngredientExp
                    | IngredientQuantity Quantity IngredientLit
                    | IngredientName IngredientLit
                    | IngredientAction Action [IngredientExp]
                    deriving Show
data IngredientLit  = IngredientLit String deriving Show

data DefaultQuantityDecl = DefaultQuantityDecl Quantity IngredientLit
                         deriving Show

data Action     = Action String [(String, String)] deriving Show
data ActionDecl = ActionDecl String Action deriving Show

data Quantity = Count (Ratio Integer)
              | Amount (Ratio Integer) Unit
              deriving Show
              
data Unit     = Unit String deriving Show
data UnitDecl = UnitDecl (Ratio Integer) String (Ratio Integer) Unit
              deriving Show

