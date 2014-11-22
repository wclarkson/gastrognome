{-# LANGUAGE QuasiQuotes #-}

module Tools.ShoppingTest where

import Data.Ratio

import Language.GastroGnome.Syntax
import Language.GastroGnome.Quote
import Tools.ShoppingHelper

[gastrognome|
9/4 cups Flour
1 tsp Baking Soda
1 tsp Salt
1 cup Butter
3/4 cup Granulated Sugar
3/4 cup Brown Sugar
1 tsp Vanilla Extract
2 Egg
2 cups Chocolate Chips

Prepared Oven:
  PREHEAT to "375 F"
    Oven

Dry Ingredients:
  MIX
    Flour
    Baking Soda
    Salt

Wet Ingredients:
  BEAT
    BEAT
      Butter
      Granulated Sugar
      Brown Sugar
      Vanilla Extract
    Egg

Cookie Dough:
  STIR
    BEAT
      Wet Ingredients
      Dry Ingredients
    Chocolate Chips

Cookies:
  COOL for "2 min"
    BAKE for "9-11 min" in "Prepared Oven"
      SEPARATE by "1 tbsp"
            Cookie Dough
|]

amntDecl :: Ratio Integer -> String -> String -> DefaultQuantityDecl
amntDecl ratio unit name = DefaultQuantityDecl (Amount ratio (Unit unit)) (IngredientLit name)

countDecl :: Ratio Integer -> String -> DefaultQuantityDecl
countDecl ratio name = DefaultQuantityDecl (Count ratio) (IngredientLit name)

pantry :: [DefaultQuantityDecl]
pantry = [ amntDecl (3%4) "cup" "Brown Sugar"
         , amntDecl (2) "cup" "Butter"]

needs :: [IngredientExp]
needs = [cookies]

whatDoINeed :: [DefaultQuantityDecl]
whatDoINeed = makeShoppingList pantry needs

printList :: [DefaultQuantityDecl] -> IO ()
printList [] = return ()
printList (l:ls) = do
  putStrLn (show l)
  printList ls

test :: IO ()
test = printList whatDoINeed
