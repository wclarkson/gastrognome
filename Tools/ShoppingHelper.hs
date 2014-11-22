{-# LANGUAGE QuasiQuotes #-}

module Tools.ShoppingHelper where

import Language.GastroGnome.Syntax
import Language.GastroGnome.Quote

makeShoppingList :: [DefaultQuantityDecl] -> [IngredientExp] -> [DefaultQuantityDecl]
makeShoppingList _      []      = []
makeShoppingList pantry needs = declDiff pantry (extractQuantities needs)

extractQuantities :: [IngredientExp] -> [DefaultQuantityDecl]
extractQuantities exprs =
  let extract :: [IngredientExp] -> [DefaultQuantityDecl] -> [DefaultQuantityDecl]
      extract ([])                               quants = quants
      extract ((IngredientQuantity q name):rest) quants =
        let decl = DefaultQuantityDecl q name
        in extract rest (decl:quants)
      extract ((IngredientAction _ exps):rest)   quants =
        extract (exps ++ rest) quants
      extract (_)                                quants = quants
  in extract exprs []

declDiff :: [DefaultQuantityDecl] -> [DefaultQuantityDecl] -> [DefaultQuantityDecl]
declDiff have need =
  let getQuant :: IngredientLit -> [DefaultQuantityDecl] -> Quantity
      getQuant _    ([])                                = Count 0
      getQuant term ((DefaultQuantityDecl q lit):decls) =
        if term == lit
        then q
        else getQuant term decls
      diff :: [DefaultQuantityDecl] -> [DefaultQuantityDecl] -> [DefaultQuantityDecl]
      diff ([])   list = list
      diff (n:ns) list =
        let DefaultQuantityDecl _ lit = n
            needed = (getQuant lit need) - (getQuant lit have)
        in case needed
             of Count  0   -> diff ns list
                Amount 0 _ -> diff ns list
                _          -> diff ns ((DefaultQuantityDecl needed lit):list)
  in diff need []
