module Util exposing (isJust, sequence, singleton)

import Maybe
import List
import Dict exposing (Dict)
import Trampoline exposing (..)


isJust: Maybe a -> Bool
isJust m = 
  case m of 
    Just _ -> True
    Nothing -> False


singleton: a -> List a
singleton x = [x]


sequence: List (Maybe a) -> Maybe (List a)
sequence xs = evaluate (sequence' xs [])


sequence': List (Maybe a) -> List a -> Trampoline (Maybe (List a))
sequence' xs acc = 
  case xs of
    [] -> done (Just (List.reverse acc))
    Just x :: xs -> jump (\_ -> sequence' xs (x :: acc))
    Nothing :: xs -> done Nothing
