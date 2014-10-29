module Examples.Waffles where

import Syntax

import Data.Ratio


wafflesText =
  unlines [
    "Dry Ingredients:",
    "  MIX",
    "    2 cup Flour",
    "    1 tablespoon Sugar",
    "    4 teaspoon Baking Powder",
    "    1/4 tsp Salt",
    " ",
    "Batter:",
    "  BEAT in \"large bowl\" until \"smooth\"",
    "    BEAT until \"fluffy\"",
    "      2 Egg",
    "    Dry Ingredients",
    "    7/4 cup Milk",
    "    1/2 cup Vegetable Oil",
    "    1/2 tsp Vanilla",
    " ",
    "Prepared Waffle Iron:",
    "  SPRAY with \"Cooking Spray\"",
    "    PREHEAT for \"15 min\"",
    "      Waffle Iron",
    " ",
    "Waffles:",
    "  COOK at \"300 F\" in \"Prepared Waffle Iron\" until \"golden brown\"",
    "    Batter"
  ]

wafflesAST = Program [
  PIngredientDecl dryIngredients,
  PIngredientDecl batter,
  PIngredientDecl preparedWaffleIron,
  PIngredientDecl waffles
  ]

flour = IngredientQuantity
          (Amount 2 (Unit "cup"))
          (IngredientLit "Flour")
sugar = IngredientQuantity
          (Amount 1 (Unit "tablespoon"))
          (IngredientLit "Sugar")
bakingPowder = IngredientQuantity
          (Amount 4 (Unit "teaspoon"))
          (IngredientLit "Baking Powder")
salt = IngredientQuantity
          (Amount (1%4) (Unit "tsp"))
          (IngredientLit "Salt")
milk = IngredientQuantity
          (Amount (7%4) (Unit "cup"))
          (IngredientLit "Milk")
vegetableOil = IngredientQuantity
          (Amount (1%2) (Unit "cup"))
          (IngredientLit "Vegetable Oil")
vanilla = IngredientQuantity
          (Amount (1%2) (Unit "tsp"))
          (IngredientLit "Vanilla")

dryIngredients =
  IngredientDecl (IngredientLit "Dry Ingredients")
    (IngredientAction
      (Action "MIX" [])
      [
        flour,
        sugar,
        bakingPowder,
        salt
      ])

batter =
  IngredientDecl (IngredientLit "Batter")
    (IngredientAction
      (Action "BEAT" [("in", "large bowl"), ("until", "smooth")])
      [
        (IngredientAction
          (Action "BEAT" [("until", "fluffy")])
          [IngredientQuantity (Count 2) (IngredientLit "Egg")]),
        IngredientName $ IngredientLit "Dry Ingredients",
        milk,
        vegetableOil,
        vanilla
      ])


preparedWaffleIron =
  IngredientDecl (IngredientLit "Prepared Waffle Iron")
    (IngredientAction
      (Action "SPRAY" [("with", "Cooking Spray")])
      [
        (IngredientAction
          (Action "PREHEAT" [("for", "15 min")])
          [
            IngredientName $ IngredientLit "Waffle Iron"
          ]
        )
      ])

waffles =
  IngredientDecl (IngredientLit "Waffles")
    (IngredientAction
      (Action "COOK" [("at", "300 F"), ("in", "Prepared Waffle Iron"), ("until", "golden brown")])
      [IngredientName $ IngredientLit "Batter"])
