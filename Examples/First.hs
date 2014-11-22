{-# LANGUAGE QuasiQuotes #-}

module Examples.First where

import Language.GastroGnome.Parser
import Language.GastroGnome.Syntax
import Language.GastroGnome.Quote

import Examples.Waffles
import Examples.OldFashioned
import Examples.Cookies

import Data.Ratio

import Test.HUnit hiding (test)
import Test.HUnit.Diff

import Text.Parsec.Error
import Text.Parsec.Pos (SourcePos, initialPos)

instance Eq ParseError where
  (==) e1 e2 = (errorPos e1 == errorPos e2) &&
               (errorMessages e1 == errorMessages e2)

test = runTestTT tests

tests = TestList[ TestLabel "waffles"       waffles_test
                , TestLabel "oldFashioned"  oldFashioned_test
                , TestLabel "cookies"       cookies_test
                , TestLabel "waffles_qquote"       waffles_qquote_test
                , TestLabel "oldFashioned_qquote"  oldFashioned_qquote_test
                , TestLabel "cookies_qquote"       cookies_qquote_test
                ]

mkTestCase expected seen = TestCase(expected @==? seen)

emptyPos :: SourcePos
emptyPos = initialPos ""

{-
  Parser Tests
-}
--Test for Waffles recipe
waffles_result  = parse parseProgram emptyPos wafflesText
waffles_expects = Right wafflesAST
waffles_test    = mkTestCase waffles_expects waffles_result

--Test for OldFashioned recipe
oldFashioned_result  = parse parseProgram emptyPos oldFashionedText
oldFashioned_expects = Right oldFashionedAST
oldFashioned_test    = mkTestCase oldFashioned_expects oldFashioned_result

--Test for Cookies recipe
cookies_result  = parse parseProgram emptyPos cookiesText
cookies_expects = Right cookiesAST
cookies_test    = mkTestCase cookies_expects cookies_result


{-
  QuasiQuoter Tests
-}

--QuasiQuoter Test for Waffles recipe
waffles_qquote_result  = Examples.First.waffles
waffles_qquote_expects =
  IngredientAction
  (Action "COOK"
     [("at", "300 F"), ("in", "Prepared Waffle Iron"),
      ("until", "golden brown")])
  [IngredientAction
     (Action "BEAT" [("in", "large bowl"), ("until", "smooth")])
     [IngredientAction (Action "BEAT" [("until", "fluffy")])
        [IngredientQuantity (Count (2 % 1)) (IngredientLit "Egg")],
      IngredientAction (Action "MIX" [])
        [IngredientQuantity (Amount (2 % 1) (Unit "cup"))
           (IngredientLit "Flour"),
         IngredientQuantity (Amount (1 % 1) (Unit "tablespoon"))
           (IngredientLit "Sugar"),
         IngredientQuantity (Amount (4 % 1) (Unit "teaspoon"))
           (IngredientLit "Baking Powder"),
         IngredientQuantity (Amount (1 % 4) (Unit "tsp"))
           (IngredientLit "Salt")],
      IngredientQuantity (Amount (7 % 4) (Unit "cup"))
        (IngredientLit "Milk"),
      IngredientQuantity (Amount (1 % 2) (Unit "cup"))
        (IngredientLit "Vegetable Oil"),
      IngredientQuantity (Amount (1 % 2) (Unit "tsp"))
        (IngredientLit "Vanilla")]]
waffles_qquote_test    = mkTestCase waffles_qquote_expects waffles_qquote_result

--QuasiQuoter Test for OldFashioned recipe
oldFashioned_qquote_result  = Examples.First.oldFashioned
oldFashioned_qquote_expects =
  IngredientAction (Action "STIR" [("in", "old-fashioned glass")])
    [IngredientName (IngredientLit "Ice Cubes"),
     IngredientAction (Action "POUR" [])
       [IngredientQuantity (Amount (1 % 1) (Unit "shot"))
          (IngredientLit "Bourbon"),
        IngredientAction (Action "MUDDLE" [("until", "paste")])
          [IngredientAction (Action "MIX" [])
             [IngredientQuantity (Amount (1 % 1) (Unit "tsp"))
                (IngredientLit "Sugar"),
              IngredientQuantity (Amount (1 % 1) (Unit "splash"))
                (IngredientLit "Water"),
              IngredientQuantity (Amount (2 % 1) (Unit "dash"))
                (IngredientLit "Angostura Bitters")],
           IngredientQuantity (Count (1 % 1))
             (IngredientLit "Maraschino Cherry"),
           IngredientQuantity (Count (1 % 1))
             (IngredientLit "Orange Wedge")]]]
oldFashioned_qquote_test    = mkTestCase oldFashioned_qquote_expects oldFashioned_qquote_result

--QuasiQuoter Test for Cookies recipe
cookies_qquote_result  = Examples.First.cookies
cookies_qquote_expects =
  IngredientAction (Action "COOL" [("for", "2 min")])
    [IngredientAction
      (Action "BAKE" [("for", "9-11 min"), ("in", "Prepared Oven")])
      [IngredientAction (Action "SEPARATE" [("by", "1 tbsp")])
         [IngredientAction (Action "STIR" [])
            [IngredientAction (Action "BEAT" [])
               [IngredientAction (Action "BEAT" [])
                  [IngredientAction (Action "BEAT" [])
                     [IngredientQuantity (Amount (1 % 1) (Unit "cup"))
                        (IngredientLit "Butter"),
                      IngredientQuantity (Amount (3 % 4) (Unit "cup"))
                        (IngredientLit "Granulated Sugar"),
                      IngredientQuantity (Amount (3 % 4) (Unit "cup"))
                        (IngredientLit "Brown Sugar"),
                      IngredientQuantity (Amount (1 % 1) (Unit "tsp"))
                        (IngredientLit "Vanilla Extract")],
                   IngredientQuantity (Count (2 % 1)) (IngredientLit "Egg")],
                IngredientAction (Action "MIX" [])
                  [IngredientQuantity (Amount (9 % 4) (Unit "cups"))
                     (IngredientLit "Flour"),
                   IngredientQuantity (Amount (1 % 1) (Unit "tsp"))
                     (IngredientLit "Baking Soda"),
                   IngredientQuantity (Amount (1 % 1) (Unit "tsp"))
                     (IngredientLit "Salt")]],
             IngredientQuantity (Amount (2 % 1) (Unit "cups"))
               (IngredientLit "Chocolate Chips")]]]]
cookies_qquote_test    = mkTestCase cookies_qquote_expects cookies_qquote_result

-- QuasiQuotes for tests
[gastrognome|
Dry Ingredients:
  MIX
    2 cup Flour
    1 tablespoon Sugar
    4 teaspoon Baking Powder
    1/4 tsp Salt

Batter:
  BEAT in "large bowl" until "smooth"
    BEAT until "fluffy"
      2 Egg
    Dry Ingredients
    7/4 cup Milk
    1/2 cup Vegetable Oil
    1/2 tsp Vanilla
 
Prepared Waffle Iron:
  SPRAY with "Cooking Spray"
    PREHEAT for "15 min"
      Waffle Iron

Waffles:
  COOK at "300 F" in "Prepared Waffle Iron" until "golden brown"
    Batter



1 shot = 2 floz
1 dash = 1/32 floz
1 splash = 4 dash

Flavor Mix:
  MUDDLE until "paste"
    MIX
      1 tsp Sugar
      1 splash Water
      2 dash Angostura Bitters
    1 Maraschino Cherry
    1 Orange Wedge

Old Fashioned:
  STIR in "old-fashioned glass"
    Ice Cubes
    POUR
      1 shot Bourbon
      Flavor Mix



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
