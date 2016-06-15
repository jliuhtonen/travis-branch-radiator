module Util exposing (isJust, singleton)

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
