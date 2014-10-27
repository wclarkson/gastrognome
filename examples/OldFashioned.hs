import Syntax

sugar =
  IngredientQuantity (Volume (Unit "tsp")  1 1) (IngredientLit "Sugar")
water =
  IngredientQuantity (Volume (Unit "splash")  3 1) (IngredientLit "Water")
bitters = IngredientQuantity
            (Volume (Unit "dash")  2 1)
            (IngredientLit "Angostura Bitters")
cherry = IngredientQuantity (Count 1 1) (IngredientLit "Cherry")
orange = IngredientQuantity (Count 1 1) (IngredientLit "Orange Wedge")

flavorMix :: IngredientDecl
flavorMix = IngredientDecl
              "Flavor Mix"
              (IngredientList [
                IngredientAction
                  (SimpleAction
                    "MUDDLE" 
                    (IngredientList [
                      IngredientAction
                        (SimpleAction
                          "MIX"
                          (IngredientList [ sugar, water, bitters ])),
                      cherry,
                      orange
                    ]))
                ])
              

bourbon = IngredientQuantity
            (Volume (Unit "shot") 1 1)
            (IngredientLit "Bourbon")
iceCubes = IngredientQuantity (Count 1 1) (IngredientLit "Ice Cube")


oldFashioned :: IngredientDecl
oldFashioned =
  IngredientDecl
    "Old Fashioned"
    (IngredientAction
      (SimpleAction
        "STIR"
        (IngredientList [
          iceCubes,
          IngredientAction
            (SimpleAction
              "POUR"
              (IngredientList [ bourbon, IngredientName (IngredientLit "Flavor Mix") ]))
        ])))
