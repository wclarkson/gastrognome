{-# LANGUAGE TemplateHaskell #-}

module Language.GastroGnome.CodeGen where

import Language.GastroGnome.Syntax
import Language.GastroGnome.Util

import Language.Haskell.TH

import Control.Monad

import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe

import Debug.Trace

-- The environment in which a recipe is interpreted, i.e., the kitchen
data Kitchen = Kitchen { ingredientEnv :: Map.Map String IngredientExp
                       , actionEnv :: Map.Map String Action
                       , quantityEnv :: Map.Map String Quantity
                       }

emptyKitchen :: Kitchen
emptyKitchen = Kitchen Map.empty Map.empty Map.empty

mkSubst :: Kitchen -> IngredientExp -> IngredientExp
mkSubst (Kitchen ie _ qe) name@(IngredientName (IngredientLit lit)) =
  case (Map.lookup lit ie, Map.lookup lit qe)
    of (Just e, Just q)    -> q # e
       (Just e, Nothing)   -> e
       (Nothing, Just q) -> IngredientQuantity q (IngredientLit lit)
       _                   -> name
mkSubst (Kitchen ie _ _) quantity@(IngredientQuantity q (IngredientLit lit)) =
  case Map.lookup lit ie
    of Just exp -> q # exp
       Nothing  -> quantity
mkSubst k@(Kitchen _ ae _) (IngredientAction a@(Action verb adverbs) exps) =
  let exps'     = map (mkSubst k) exps
      newAction =
        case Map.lookup verb ae
          of Just (Action verb' adverbs') -> Action verb' (adverbs'++adverbs)
             Nothing                      -> a
  in IngredientAction newAction exps'

genBindings :: [ProgramDecl] -> Kitchen -> Kitchen
genBindings (d:ds) k@(Kitchen ie ae qe) =
  let addKey (k,v) m = Map.insert k v m
      subst = mkSubst k
      getIngredient (IngredientDecl (IngredientLit name) exp) =
        (name, subst exp)
      getAction (ActionDecl name action) = (name, action)
      getQuantity (DefaultQuantityDecl q (IngredientLit name)) = (name, q)
      k'    = case d of
                PIngredientDecl id       ->
                  Kitchen (addKey (getIngredient id) ie) ae qe
                PActionDecl ad           ->
                  Kitchen ie (addKey (getAction ad) ae) qe
                PDefaultQuantityDecl dqd -> 
                  Kitchen ie ae (addKey (getQuantity dqd) qe)
                _                        ->
                  Kitchen ie ae qe

  in genBindings ds k'
genBindings [] k = k

reifyRecipe :: Program -> [IngredientDecl]
reifyRecipe (Program decls) =
  let (Kitchen ie ae qe) = genBindings decls emptyKitchen
      mkIngredientDecl name ie ingDecls =
        (IngredientDecl (IngredientLit name) ie):ingDecls
  in Map.foldrWithKey mkIngredientDecl [] ie

lowerName:: IngredientLit -> String
lowerName (IngredientLit name) = 
  case concat (words name) of (l:ls) -> (toLower l) : ls
                              []     -> []

makeValDecl :: String -> ExpQ -> Q [Dec]
makeValDecl name exp = do
  { e <- exp
  ; return $ [ ValD (VarP (mkName name)) (NormalB e) [] ]
  }

-- Must take at least one exp
makeConApp :: String -> [Exp] -> Exp
makeConApp conName exps = foldr (flip AppE) (ConE (mkName conName)) exps

makeString :: String -> Exp
makeString = LitE . StringL


concatQ :: [Q [a]] -> Q [a]
concatQ l = foldr (liftM2 (++)) (return []) l

makeIngredientDecls :: [IngredientDecl] -> Q [Dec]
makeIngredientDecls decls =
  let f (IngredientDecl lit exp) = makeValDecl (lowerName lit) [| exp |]
  in concatQ $ map f decls

makeGastroGnomeDecls :: Program -> Q [Dec]
makeGastroGnomeDecls p = 
  let r = reifyRecipe p
      d = makeIngredientDecls r
  in d

makeGastroGnomeExp :: IngredientExp -> Q Exp
makeGastroGnomeExp ie = [| ie |]


