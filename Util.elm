module Util(isJust, sequence, singleton) where

import Maybe
import List
import Trampoline exposing (..)

isJust: Maybe a -> Bool
isJust m = case m of
  Just _ -> True
  Nothing -> False

singleton: a -> List a
singleton x = [x]

sequence: List (Maybe a) -> Maybe (List a)
sequence xs = trampoline (sequence' xs [])

sequence': List (Maybe a) -> List a -> Trampoline (Maybe (List a))
sequence' xs acc = case xs of
  []            -> Done (Just (List.reverse acc))
  Just x :: xs  -> Continue (\_ -> sequence' xs (x :: acc))
  Nothing :: xs -> Done Nothing
