module RadiatorApp where
import StartApp
import Effects exposing (Never, Effects)
import Task exposing (Task)
import Time exposing (..)
import String
import Json.Decode exposing (Decoder)
import Http
import Html exposing (Html)
import Html.Attributes exposing (class, id, for, value, rows)
import Html.Events exposing (onClick)
import Travis
import Util
import Debug

import RadiatorModel as Model
import RadiatorView as View
import RadiatorUpdate as Update

app = StartApp.start { init = (Model.initialModel, Update.refreshBuilds Model.initialConfig), view = View.view, update = Update.update, inputs = [timedUpdate] }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

timedUpdate : Signal Model.Action
timedUpdate = Signal.map (\_ -> Model.RefreshBuilds) (every (30 * second))
