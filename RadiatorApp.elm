module RadiatorApp where
import StartApp
import Effects exposing (Never, Effects)
import Task exposing (Task)
import Time exposing (..)
import Json.Decode exposing (Decoder)
import Http
import Html exposing (Html)
import Html.Attributes exposing (class, id, for, value)
import Html.Events exposing (onClick)
import Travis
import Debug

defaultRepository = "elm-lang/elm-compiler"

type Action = RefreshBuilds | NewBuildStatus (Maybe Travis.BranchStatus) | FlipConfigMode | UpdateRepositoryField String | UpdateApiKeyField String | SaveConfiguration

app = StartApp.start { init = (model, refreshBuilds defaultRepository), view = view, update = update, inputs = [clock] }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

clock : Signal Action
clock = Signal.map (\_ -> RefreshBuilds) (every (30 * second))

initialConfig = { apiKey = Nothing, repository = defaultRepository }
model = Model Config initialConfig initialConfig []

type alias Model = {
  mode: AppMode,
  configuration: Configuration,
  configViewModel: Configuration,
  buildStatus : List BuildStatus
}

type alias Configuration = {
  apiKey: Maybe String,
  repository: String
}

type alias BuildStatus = {
  branch : String,
  state : String
}

type AppMode = Monitoring | Config

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
     RefreshBuilds -> (model, (refreshBuilds model.configuration.repository))
     NewBuildStatus (Just builds) -> ((refreshModelBuildState builds model), Effects.none)
     NewBuildStatus Nothing -> (model, Effects.none)
     FlipConfigMode -> ({ model | mode = (flipAppMode model.mode) }, Effects.none)
     UpdateRepositoryField repo ->
       let currentConfigView = model.configViewModel
           configView = { currentConfigView | repository = repo }
       in ({ model | configViewModel = configView }, Effects.none)
     UpdateApiKeyField key ->
       let currentConfigView = model.configViewModel
           configView = { currentConfigView | apiKey = Just key }
       in ({ model | configViewModel = configView }, Effects.none)
     SaveConfiguration -> ({ model | configuration = model.configViewModel }, (refreshBuilds model.configViewModel.repository))

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

flipAppMode: AppMode -> AppMode
flipAppMode mode =  case mode of 
  Monitoring -> Config
  Config -> Monitoring

view: Signal.Address Action -> Model -> Html
view actionAddress model =
  let
    configMarkup = if model.mode == Config 
                     then configPanel model.configuration actionAddress
                     else []
  in
     Html.div [] [
       Html.button [(class "config-button"), (onClick actionAddress FlipConfigMode)] [],
       Html.div [] configMarkup,
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

configPanel: Configuration -> Signal.Address Action -> List Html
configPanel { repository, apiKey } actionAddress = 
  let
      apiKeyValue = Maybe.withDefault "" apiKey 
  in 
     [Html.div [class "config-panel"] [
       Html.label [for "slug-field"] [Html.text "Repository slug:"],
       Html.input [(id "repository-field"), (value repository)] [],
       Html.label [for "api-key-field"] [Html.text "Private Travis API key:"],
       Html.input [(id "api-key-field"), (value apiKeyValue)] [],
       Html.button [] [ Html.text "Save" ]
       ]]
