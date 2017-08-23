module Util exposing (isJust, singleton, listFromMaybe)

import Maybe
import List
import Dict exposing (Dict)
import Trampoline exposing (..)


isJust: Maybe a -> Bool
isJust m = 
  case m of 
    Just _ -> True
    Nothing -> False


listFromMaybe: Maybe a -> List a
listFromMaybe m =
  Maybe.map singleton m |> Maybe.withDefault []


singleton: a -> List a
singleton x = [x]
