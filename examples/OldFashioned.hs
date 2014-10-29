import Syntax

oldFashionedText = 
  unlines [
    "1 shot = 2 floz"
    "1 dash = 1/32 oz"
    "1 splash = 4 dash"
    ""
    "Flavor Mix:"
    "  MUDDLE until \"paste\""
    "    MIX"
    "      1 tsp Sugar"
    "      1 splash Water"
    "      2 dash Angostura Bitters"
    "    1 Maraschino Cherry"
    "    1 Orange Wedge"
    ""
    "Old Fashioned:"
    "  STIR in \"old-fashioned glass\""
    "    Ice Cubes"
    "    POUR"
    "      1 shot Bourbon"
    "      Flavor Mix"
  ]

oldFashioendAST = [
    shot,
    dash,
    splash,
    flavorMix,
    oldFashioned
  ]

shot   = UnitDecl 1 "shot"   2      (Unit "floz")
dash   = UnitDecl 1 "dash"   (1%32) (Unit "floz")
splash = UnitDecl 1 "splash" 4      (Unit "dash")

sugar = IngredientQuantity (Amount 1 (Unit "tsp")) (IngredientLit "Sugar")
water = IngredientQuantity (Amount 1 (Unit "splash")) (IngredientLit "Water")
bitters = IngredientQuantity
            (Amount 2 (Unit "dash"))
            (IngredientLit "Angostura Bitters")
cherry = IngredientQuantity (Count 1) (IngredientLit "Cherry")
orange = IngredientQuantity (Count 1) (IngredientLit "Orange Wedge")

flavorMix :: IngredientDecl
flavorMix =
  IngredientDecl "Flavor Mix"
      (IngredientAction
        (Action "MUDDLE" [])
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


oldFashioned :: IngredientDecl
oldFashioned =
  IngredientDecl "Old Fashioned"
    (IngredientAction
          (Action "STIR" [])
          [
            iceCubes,
            IngredientAction
              (Action "POUR" [])
              [ bourbon, IngredientName (IngredientLit "Flavor Mix") ]
          ])