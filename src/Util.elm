module Util exposing (isJust, singleton)


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


singleton : a -> List a
singleton x =
    [ x ]
