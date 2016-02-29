module RadiatorApp where
import StartApp
import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)
import Time exposing (..)
import RadiatorModel as Model
import RadiatorView as View
import RadiatorUpdate as Update

app = StartApp.start { init = (Model.initialModel, Update.refreshBuilds Model.initialConfig), view = View.view, update = Update.update, inputs = [timedUpdate] }

main : Signal Html
main = app.html

timedUpdate : Signal Model.Action
timedUpdate = Signal.map (\_ -> Model.RefreshBuilds) (every (30 * second))

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

