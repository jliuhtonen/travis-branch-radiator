module RadiatorApp exposing (defaultConfig, initialConfigPanel, initialize, main, refreshInterval, timedUpdate)

import Browser
import Radiator.Model as Model
import Radiator.Update as Update
import Radiator.View as View
import Task exposing (Task)
import Time exposing (..)


refreshInterval : Float
refreshInterval =
    30 * 1000


defaultConfig =
    { apiKey = Nothing
    , repositories =
        [ "elm-lang/elm-compiler", "elm-lang/core" ]
    }


initialConfigPanel : Model.Configuration -> Model.ConfigPanel
initialConfigPanel config =
    { repositorySlug = ""
    , apiKeyValue = Maybe.withDefault "" config.apiKey
    }


main : Program { localStorageCfg : Maybe Model.Configuration } Model.Model Model.Msg
main =
    Browser.element { init = initialize, view = View.view, update = Update.update, subscriptions = \_ -> timedUpdate }


initialize : { localStorageCfg : Maybe Model.Configuration } -> ( Model.Model, Cmd Model.Msg )
initialize { localStorageCfg } =
    let
        config =
            Maybe.withDefault defaultConfig localStorageCfg

        model =
            Model.Model Model.Config config (initialConfigPanel config) []
    in
    ( model, Update.refreshBuilds config )


timedUpdate : Sub Model.Msg
timedUpdate =
    every refreshInterval (\_ -> Model.RefreshBuilds)
