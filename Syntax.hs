module Syntax where
import Data.Ratio

{-
  Abstract Syntax Datatypes
-}

data Program        = Program [ProgramDecl] deriving Eq

data ProgramDecl    = PIngredientDecl IngredientDecl
                    | PDefaultQuantityDecl DefaultQuantityDecl
                    | PActionDecl ActionDecl
                    | PUnitDecl UnitDecl
                    deriving Eq

-- Ingredients
data IngredientDecl = IngredientDecl String IngredientExp deriving Eq
data IngredientExp  = IngredientQuantity Quantity IngredientLit
                    | IngredientName IngredientLit
                    | IngredientAction Action [IngredientExp]
                    deriving Eq
data IngredientLit  = IngredientLit String deriving Eq

-- Quantities
data DefaultQuantityDecl = DefaultQuantityDecl Quantity IngredientLit deriving Eq
data Quantity = Count (Ratio Integer)
              | Amount (Ratio Integer) Unit
              deriving Eq

-- Actions
data Action     = Action String [(String, String)] deriving Eq
data ActionDecl = ActionDecl String Action deriving Eq
              
-- Units
data Unit     = Unit String deriving Eq
data UnitDecl = UnitDecl (Ratio Integer) String (Ratio Integer) Unit deriving Eq


{-
  Abstract Syntax Show Instances
-}

indent :: [String] -> [String]
indent ls = map ("\t"++) ls

instance Show Program where
  show (Program decls) = unlines $ map show decls

instance Show ProgramDecl where
  show (PIngredientDecl d) = show d
  show (PDefaultQuantityDecl d) = show d
  show (PActionDecl d) = show d
  show (PUnitDecl d) = show d

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

showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio r = 
  let num = numerator r
      den = denominator r
  in show num ++ if (den == 1)
                   then ""
                   else "/" ++ show den

instance Show DefaultQuantityDecl where
  show (DefaultQuantityDecl quantity ingredient) =
    unwords [show quantity, show ingredient]

instance Show Quantity where
  show (Count r) = showRatio r
  show (Amount r unit) = unwords [showRatio r, show unit]

instance Show Action where
  show (Action verb adverbs) =
    let showAdverb (conn, adv) = unwords [conn, "\"" ++ adv ++ "\""]
    in unwords (verb : map showAdverb adverbs)

instance Show ActionDecl where
  show (ActionDecl name action) = unwords [name, "=", show action]


instance Show Unit where
  show (Unit unit) = unit

instance Show UnitDecl where
  show (UnitDecl rLeft name rRight unit) =
    unwords [showRatio rLeft, name, "=", showRatio rRight, show unit]
