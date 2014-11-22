module Language.GastroGnome.Util where

import Language.GastroGnome.Syntax

(#) :: Quantity -> IngredientExp -> IngredientExp
(#) q1 (IngredientQuantity q2 lit) =
  IngredientQuantity (q1*q2) lit
(#) q name@(IngredientName _) = name
(#) q (IngredientAction a es) =
  IngredientAction a (map ((#) q) es)

{-
extractIngrdientList :: IngredientExp -> [IngredientExp]
extractIngrdientList exp =
  let extractHelper iq@(IngredientQuantity q lit) iqs = iq:iqs
      extractHelper (IngredientName lit) iqs = iqs
      extractHelper (IngredientAction action exps) iqs =
        concatMap extractIngrdientList exps
  in extractIngrdientList exp []
-}

