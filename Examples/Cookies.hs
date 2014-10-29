module Examples.Cookies where

import Syntax

import Data.Ratio


cookiesText =
  unlines [
    "9/4 cups Flour",
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
    "      SEPARATE by \"1 tbsp\"",
    "        Cookie Dough"
  ]

cookiesAST = Program [
  PDefaultQuantityDecl flourDefault,
  PDefaultQuantityDecl bakingSodaDefault,
  PDefaultQuantityDecl saltDefault,
  PDefaultQuantityDecl butterDefault,
  PDefaultQuantityDecl granulatedSugarDefault,
  PDefaultQuantityDecl brownSugarDefault,
  PDefaultQuantityDecl vanillaExtractDefault,
  PDefaultQuantityDecl eggsDefault,
  PDefaultQuantityDecl chocolateChipsDefault,
  PIngredientDecl preparedOven,
  PIngredientDecl dryIngredients,
  PIngredientDecl wetIngredients,
  PIngredientDecl cookieDough,
  PIngredientDecl cookies
  ]

flour           = IngredientName $ IngredientLit "Flour"
bakingSoda      = IngredientName $ IngredientLit "Baking Soda"
salt            = IngredientName $ IngredientLit "Salt"
butter          = IngredientName $ IngredientLit "Butter"
granulatedSugar = IngredientName $ IngredientLit "Granulated Sugar"
brownSugar      = IngredientName $ IngredientLit "Brown Sugar"
vanillaExtract  = IngredientName $ IngredientLit "Vanilla Extract"
eggs            = IngredientName $ IngredientLit "Egg"
chocolateChips  = IngredientName $ IngredientLit "Chocolate Chips"

flourDefault = DefaultQuantityDecl
          (Amount (2 + 1%4) (Unit "cups"))
          (IngredientLit "Flour")
bakingSodaDefault = DefaultQuantityDecl
          (Amount 1 (Unit "tsp"))
          (IngredientLit "Baking Soda")
saltDefault = DefaultQuantityDecl
          (Amount 1 (Unit "tsp"))
          (IngredientLit "Salt")
butterDefault = DefaultQuantityDecl
          (Amount 1 (Unit "cup"))
          (IngredientLit "Butter")
granulatedSugarDefault = DefaultQuantityDecl
          (Amount (3%4) (Unit "cup"))
          (IngredientLit "Granulated Sugar")
brownSugarDefault = DefaultQuantityDecl
          (Amount (3%4) (Unit "cup"))
          (IngredientLit "Brown Sugar")
vanillaExtractDefault = DefaultQuantityDecl
          (Amount 1 (Unit "tsp"))
          (IngredientLit "Vanilla Extract")
eggsDefault = DefaultQuantityDecl
          (Count 2)
          (IngredientLit "Egg")
chocolateChipsDefault = DefaultQuantityDecl
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
  IngredientDecl (IngredientLit "Cookie Dough")
    (IngredientAction
      (Action "STIR" [])
      [
        (IngredientAction
          (Action "BEAT" [])
          [
            IngredientName $ IngredientLit "Wet Ingredients",
            IngredientName $ IngredientLit "Dry Ingredients"
          ]),
          chocolateChips
      ])

cookies =
  IngredientDecl (IngredientLit "Cookies")
    (IngredientAction
      (Action "COOL" [("for", "2 min")])
      [
        (IngredientAction
          (Action "BAKE" [("for", "9-11 min"), ("in", "Prepared Oven")])
          [
            (IngredientAction
              (Action "SEPARATE" [("by", "1 tbsp")])
              [IngredientName $ IngredientLit "Cookie Dough"])])])
