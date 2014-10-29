module Examples.OldFashioned where

import Syntax

import Data.Ratio


oldFashionedText = 
  unlines [
    "1 shot = 2 floz",
    "1 dash = 1/32 floz",
    "1 splash = 4 dash",
    "",
    "Flavor Mix:",
    "  MUDDLE until \"paste\"",
    "    MIX",
    "      1 tsp Sugar",
    "      1 splash Water",
    "      2 dash Angostura Bitters",
    "    1 Maraschino Cherry",
    "    1 Orange Wedge",
    "",
    "Old Fashioned:",
    "  STIR in \"old-fashioned glass\"",
    "    Ice Cubes",
    "    POUR",
    "      1 shot Bourbon",
    "      Flavor Mix"
  ]

oldFashionedAST = Program [
    PUnitDecl shot,
    PUnitDecl dash,
    PUnitDecl splash,
    PIngredientDecl flavorMix,
    PIngredientDecl oldFashioned
  ]

shot   = UnitDecl 1 "shot"   2      (Unit "floz")
dash   = UnitDecl 1 "dash"   (1%32) (Unit "floz")
splash = UnitDecl 1 "splash" 4      (Unit "dash")

sugar = IngredientQuantity (Amount 1 (Unit "tsp")) (IngredientLit "Sugar")
water = IngredientQuantity (Amount 1 (Unit "splash")) (IngredientLit "Water")
bitters = IngredientQuantity
            (Amount 2 (Unit "dash"))
            (IngredientLit "Angostura Bitters")
cherry = IngredientQuantity (Count 1) (IngredientLit "Maraschino Cherry")
orange = IngredientQuantity (Count 1) (IngredientLit "Orange Wedge")

flavorMix =
  IngredientDecl (IngredientLit "Flavor Mix")
      (IngredientAction
        (Action "MUDDLE" [("until", "paste")])
        [
          IngredientAction
            (Action "MIX" [])
            [ sugar, water, bitters ],
          cherry,
          orange
        ])


bourbon = IngredientQuantity
            (Amount 1 (Unit "shot"))
            (IngredientLit "Bourbon")
iceCubes = IngredientQuantity (Count 1) (IngredientLit "Ice Cube")


oldFashioned =
  IngredientDecl (IngredientLit "Old Fashioned")
    (IngredientAction
          (Action "STIR" [("in", "old-fashioned glass")])
          [
            IngredientName $ IngredientLit "Ice Cubes",
            IngredientAction
              (Action "POUR" [])
              [ bourbon, IngredientName (IngredientLit "Flavor Mix") ]
          ])