module RadiatorApp where
import StartApp
import Effects exposing (Never, Effects)
import Task exposing (Task)
import Time exposing (..)
import Json.Decode exposing (..)
import Http
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Travis
import Debug

defaultRepository = "elm-lang/elm-compiler"

type Action = RefreshBuilds | NewBuildStatus (Maybe Travis.BranchStatus)

app = StartApp.start { init = (model, refreshBuilds defaultRepository), view = view, update = update, inputs = [clock] }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

clock : Signal Action
clock = Signal.map (\_ -> RefreshBuilds) (every (30 * second))

model = Model Config Nothing []

type alias Model = {
  mode: AppMode,
  apiKey: Maybe String,
  buildStatus : List BuildStatus
}

type alias BuildStatus = {
  branch : String,
  state : String
}

type AppMode = Monitoring | Config

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
     RefreshBuilds -> (model, (refreshBuilds defaultRepository))
     NewBuildStatus (Just builds) -> ((refreshModelBuildState builds model), Effects.none)
     NewBuildStatus Nothing -> (model, Effects.none)

refreshModelBuildState: Travis.BranchStatus -> Model -> Model 
refreshModelBuildState updatedBranchStatus model =
  let updatedBuildStatus = toBuildStatusList updatedBranchStatus
  in { model | buildStatus = (Debug.log "build status" updatedBuildStatus) }

toBuildStatusList: Travis.BranchStatus -> List BuildStatus
toBuildStatusList {branches, commits} = 
  List.map2 combineAsBuildStatus branches commits

combineAsBuildStatus: Travis.BranchBuild -> Travis.Commit -> BuildStatus
combineAsBuildStatus { state } { branch } = { state = state, branch = branch }

refreshBuilds : String -> Effects Action 
refreshBuilds repositorySlug =
  Travis.getBranchBuildStatus repositorySlug
    |> Task.map NewBuildStatus
    |> Effects.task

view address model =
  Html.div [] [
    Html.button [(class "config-button")] [],
    Html.ul [(class "branch-list")] (buildListing model.buildStatus)
    ] 

buildListing: List BuildStatus -> List Html
buildListing statuses = List.take 5 statuses |> List.map asListItem

asListItem: BuildStatus -> Html
asListItem s = Html.li [class ("branch " ++ s.state)] (branchElems s) 

branchElems: BuildStatus -> List Html
branchElems { branch } = [
    (Html.span [class "branch-name"] [Html.text branch])
  ]
