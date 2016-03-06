module RadiatorApp where
import StartApp
import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)
import Time exposing (..)
import RadiatorModel as Model
import RadiatorView as View
import RadiatorUpdate as Update

defaultConfig = { apiKey = Nothing, repositories =
  ["elm-lang/elm-compiler", "elm-lang/core"] }

config = Maybe.withDefault defaultConfig loadConfiguration
initialConfigPanel = { repositorySlug = "", apiKey = "" }
initialModel = Model.Model Model.Config config initialConfigPanel []

app = StartApp.start { init = (initialModel, Update.refreshBuilds config), view = View.view, update = Update.update, inputs = [timedUpdate] }

main : Signal Html
main = app.html

timedUpdate : Signal Model.Action
timedUpdate = Signal.map (\_ -> Model.RefreshBuilds) (every (30 * second))

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

port loadConfiguration: Maybe Model.Configuration

port saveConfiguration: Signal Model.Configuration
port saveConfiguration = Signal.map (\model -> model.configuration) app.model
