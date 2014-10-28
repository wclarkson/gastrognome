module Syntax where
import Data.Ratio

data IngredientDecl = IngredientDecl String IngredientExp
data IngredientExp  = IngredientQuantity Quantity IngredientLit
                    | IngredientName IngredientLit
                    | IngredientAction Action [IngredientExp]
data IngredientLit  = IngredientLit String

data DefaultQuantityDecl = DefaultQuantityDecl Quantity IngredientLit
                         deriving Show

data Action     = Action String [(String, String)]
data ActionDecl = ActionDecl String Action

data Quantity = Count (Ratio Integer)
              | Amount (Ratio Integer) Unit
              
data Unit     = Unit String
data UnitDecl = UnitDecl (Ratio Integer) String (Ratio Integer) Unit

indent :: [String] -> [String]
indent ls = map ("\t"++) ls

instance Show IngredientDecl where
  show (IngredientDecl name exp) =
    unlines $ (name ++ ":") : (indent (lines $ show exp))

instance Show IngredientExp where
  show (IngredientQuantity quantity lit) = unwords [show quantity, show lit]
  show (IngredientName lit) = show lit
  show (IngredientAction action exps) =
    unlines $ (show action) : (indent (concatMap (lines . show) exps))

instance Show IngredientLit where
  show (IngredientLit lit) = lit

instance Show Action where
  show (Action verb adverbs) =
    let showAdverb (conn, adv) = unwords [conn, "\"" ++ adv ++ "\""]
    in unwords (verb : map showAdverb adverbs)

instance Show ActionDecl where
  show (ActionDecl name action) = unwords [name, "=", show action]

showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio r = 
  let num = numerator r
      den = denominator r
  in show num ++ if (den == 1)
                   then ""
                   else "/" ++ show den

instance Show Quantity where
  show (Count r) = showRatio r
  show (Amount r unit) = unwords [showRatio r, show unit]

instance Show Unit where
  show (Unit unit) = unit

instance Show UnitDecl where
  show (UnitDecl rLeft name rRight unit) =
    unwords [showRatio rLeft, name, "=", showRatio rRight, show unit]

