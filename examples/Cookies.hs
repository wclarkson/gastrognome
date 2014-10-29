module Examples.Cookies where

import Syntax

import Data.Ratio


cookiesText =
  unlines [
    "2 1/4 cups Flour",
    "1 tsp Baking Soda",
    "1 tsp Salt",
    "1 cup Butter",
    "3/4 cup Granulated Sugar",
    "3/4 cup Brown Sugar",
    "1 tsp Vanilla Extract",
    "2 Egg",
    "2 cups Chocolate Chips",

    "Prepared Oven:",
    "  PREHEAT to \"375 F\"",
    "    Oven",

    "Dry Ingredients:",
    "  MIX",
    "    Flour",
    "    Baking Soda",
    "    Salt",

    "Wet Ingredients:",
    "  BEAT",
    "    BEAT",
    "      Butter",
    "      Granulated Sugar",
    "      Brown Sugar",
    "      Vanilla Extract",
    "    Egg",

    "Cookie Dough:",
    "  STIR",
    "    BEAT",
    "      Wet Ingredients",
    "      Dry Ingredients",
    "    Chocolate Chips",

    "Cookies:",
    "  COOL for \"2 min\"",
    "    BAKE for \"9-11 min\" in \"Prepared Oven\"",
    "      SEPARATE by 1 tbsp",
    "        Cookie Dough"
  ]

cookiesAST = Program [
  PIngredientDecl preparedOven,
  PIngredientDecl dryIngredients,
  PIngredientDecl wetIngredients,
  PIngredientDecl cookieDough,
  PIngredientDecl cookies
  ]

flour = IngredientQuantity
          (Amount (2 + 1%4) (Unit "cups"))
          (IngredientLit "Flour")
bakingSoda = IngredientQuantity
          (Amount 1 (Unit "tsp"))
          (IngredientLit "Baking Soda")
salt = IngredientQuantity
          (Amount 1 (Unit "tsp"))
          (IngredientLit "Salt")
butter = IngredientQuantity
          (Amount 1 (Unit "cup"))
          (IngredientLit "Butter")
granulatedSugar = IngredientQuantity
          (Amount (3%4) (Unit "cup"))
          (IngredientLit "Granulated Sugar")
brownSugar = IngredientQuantity
          (Amount (3%4) (Unit "cup"))
          (IngredientLit "Brown Sugar")
vanillaExtract = IngredientQuantity
          (Amount 1 (Unit "tsp"))
          (IngredientLit "Vanilla Extract")
eggs = IngredientQuantity
          (Count 2)
          (IngredientLit "Egg")
chocolateChips = IngredientQuantity
          (Amount 2 (Unit "cups"))
          (IngredientLit "Chocolate Chips")


preparedOven =
  IngredientDecl (IngredientLit "Prepared Oven")
    (IngredientAction
      (Action "PREHEAT" [("to", "375 F")])
      [IngredientName $ IngredientLit "Oven"])

dryIngredients =
  IngredientDecl (IngredientLit "Dry Ingredients")
    (IngredientAction
      (Action "MIX" [])
      [
        flour,
        bakingSoda,
        salt
      ])

wetIngredients =
  IngredientDecl (IngredientLit "Wet Ingredients")
    (IngredientAction
      (Action "BEAT" [])
      [
        (IngredientAction
          (Action "BEAT" [])
          [
            butter,
            granulatedSugar,
            brownSugar,
            vanillaExtract
          ]),
        eggs
      ])

cookieDough =
  let IngredientDecl _ wet = wetIngredients
      IngredientDecl _ dry = dryIngredients
  in
  IngredientDecl (IngredientLit "Cookie Dough")
    (IngredientAction
      (Action "STIR" [])
      [
        (IngredientAction
          (Action "BEAT" [])
          [
            wet,
            dry
          ]),
          chocolateChips
      ])

cookies =
  let IngredientDecl _ dough = cookieDough in
  IngredientDecl (IngredientLit "Cookies")
    (IngredientAction
      (Action "Cool" [("for", "2 min")])
      [
        (IngredientAction
          (Action "BAKE" [("for", "9-11 min"), ("in", "preparedOven")])
          [
            (IngredientAction
              (Action "SEPARATE" [("by", "1 tbsp")])
              [dough])])])
