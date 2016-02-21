module Util(sequence) where

import Maybe
import List
import Trampoline exposing (..)

sequence: List (Maybe a) -> Maybe (List a)
sequence xs = trampoline (sequence' xs [])

sequence': List (Maybe a) -> List a -> Trampoline (Maybe (List a))
sequence' xs acc = case xs of
  []            -> Done (Just (List.reverse acc))
  Just x :: xs  -> Continue (\_ -> sequence' xs (x :: acc))
  Nothing :: xs -> Done Nothing
