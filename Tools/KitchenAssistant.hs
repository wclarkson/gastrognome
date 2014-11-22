{-# LANGUAGE QuasiQuotes #-}

module Tools.KitchenAssistant where

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

{-
genTL :: IngredientExp -> [(Time, Action, String)]
genTL exp =
  let genTLRec :: Time -> IngredientExp -> ([(Time, Action, String)], Time)
      genTLRec start (IngredientAction action ies) =
        let f (IngredientAction a kids) (list, time) =
              let children = map (genTLRec time) kids
              in ((time, a, show a):(children++list), addTime time (getTime a))
            f _ tup = tup
        in foldr f ([], start) ies
      genTLRec start                         _     = ([], start)
      (list, _) = genTLRec 0 exp
  in list
  -}

type TLEntry = (Time, Action, String)

genTimeline :: IngredientExp -> [TLEntry]
genTimeline exp =
  let genTL :: IngredientExp -> [TLEntry] -> [TLEntry]
      genTL (IngredientAction action exps) list =
        foldr genTL ((Seconds 0, action, ""):list) exps
      genTL _                                 x = x
      updateTime :: (Time, [TLEntry]) -> TLEntry -> (Time, [TLEntry])
      updateTime (start, list) (_, a, s) =
        (addTime start (getTime a), (start, a, s):list)
      (_,list) = foldl updateTime (Seconds 0, []) $ genTL exp []
  in reverse list

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




