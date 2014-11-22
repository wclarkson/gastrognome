{-# LANGUAGE QuasiQuotes #-}

module Tools.KitchenAssistant where

import Data.List

import Language.GastroGnome.Quote
import Language.GastroGnome.Syntax

data Time = Seconds Int deriving Show

addTime :: Time -> Time -> Time
addTime (Seconds x) (Seconds y) = Seconds (x+y)

parseTime :: String -> Time
parseTime input =
  let hours w = elem w ["h", "hr",  "hrs",  "hour",   "hours"]
      mins w  = elem w ["m", "min", "mins", "minute", "minutes"]
      secs w  = elem w ["s", "sec", "secs", "second", "seconds"]
      f (w0:w1:ws) s 
        | hours w1 = f ws $ (3600 * read w0) + s
        | mins w1  = f ws $ (60 * read w0) + s
        | secs w1  = f ws $ read w0 + s
      f [] s = s
  in Seconds $ f (words input) 0

getTime :: Action -> Time
getTime (Action _ adverbs) =
  let findTime (("for",time):_) = time
      findTime (_:rest)         = findTime rest
      findTime []               = "0 s"
  in parseTime $ findTime adverbs

type TLEntry = (Time, Action, String)

showTLEntry :: TLEntry -> String
showTLEntry (t, a, s) = unwords [show a, show s]

genIngredientName :: [IngredientExp] -> String
genIngredientName exps =
  let f q@(IngredientQuantity _ _) = [show q]
      f (IngredientName (IngredientLit name))       = [name]
      f (IngredientAction _ exps)                   = concatMap f exps
  in intercalate ", " $ concatMap f exps

genTimeline :: IngredientExp -> [TLEntry]
genTimeline exp =
  let genTL :: IngredientExp -> [TLEntry] -> [TLEntry]
      genTL (IngredientAction action exps) list =
        foldr genTL ((Seconds 0, action, genIngredientName exps):list) exps
      genTL _                                 x = x
      updateTime :: (Time, [TLEntry]) -> TLEntry -> (Time, [TLEntry])
      updateTime (start, list) (_, a, s) =
        (addTime start (getTime a), (start, a, s):list)
      (_,list) = foldl updateTime (Seconds 0, []) $ genTL exp []
  in reverse list

kitchenAssistant :: IngredientExp -> IO ()
kitchenAssistant exp = do
  { putStrLn "Hit enter after completing the step."
  ; let steps = genTimeline exp
  ; let f (x:xs) = do { putStr $ showTLEntry x; getLine; f xs }
        f []     = return ()
  ; f steps
  }

[gastrognome|
  2 Egg
  3 cup Flour

  BAKE = SHAKE with "vigor"

  Cookies:
    BAKE for "10 s"
      Egg
      MIX for "4 s"
        Flour
        Water
      SWIRL for "5 s"
        Cat
|]




