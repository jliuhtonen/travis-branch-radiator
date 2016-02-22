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

type Action = RefreshBuilds 
            | NewBuildStatus (Maybe (List (String, Travis.BranchStatus)))
            | FlipConfigMode 
            | UpdateRepositoryField (List String)
            | UpdateApiKeyField String 
            | SaveConfiguration

type alias Model = {
  mode: AppMode,
  configuration: Configuration,
  configPanel: Configuration,
  buildStatus : List RepositoryStatus
}

type alias RepositoryStatus = (String, List BuildStatus)

type alias Configuration = {
  apiKey: Maybe String,
  repositories: List String
}

type alias BuildStatus = {
  branch : String,
  state : String
}

type AppMode = Monitoring | Config

app = StartApp.start { init = (model, refreshBuilds initialConfig), view = view, update = update, inputs = [timedUpdate] }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

timedUpdate : Signal Action
timedUpdate = Signal.map (\_ -> RefreshBuilds) (every (30 * second))

initialConfig = { apiKey = Nothing, repositories = 
  ["elm-lang/elm-compiler", "elm-lang/core"] }
model = Model Config initialConfig initialConfig []

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
     RefreshBuilds -> (model, (refreshBuilds model.configuration))
     NewBuildStatus (Just builds) -> ((refreshModelBuildState builds model), Effects.none)
     NewBuildStatus Nothing -> (model, Effects.none)
     FlipConfigMode -> ({ model | mode = (flipAppMode model.mode) }, Effects.none)
     UpdateRepositoryField repositories ->
       let currentConfigView = model.configPanel
           configView = { currentConfigView | repositories = repositories }
       in ({ model | configPanel = configView }, Effects.none)
     UpdateApiKeyField key ->
       let keyModelValue = case String.trim key of
             ""  -> Nothing
             any -> Just any
           currentConfigView = model.configPanel
           configView = { currentConfigView | apiKey = keyModelValue }
       in ({ model | configPanel = configView }, Effects.none)
     SaveConfiguration -> ({ model | configuration = model.configPanel, 
        mode = Monitoring }, (refreshBuilds model.configPanel))

refreshModelBuildState: List (String, Travis.BranchStatus) -> Model -> Model 
refreshModelBuildState updatedBranchStatuses model =
  let updatedBuildStatuses = List.map toBuildStatusList updatedBranchStatuses
  in { model | buildStatus = updatedBuildStatuses }

toBuildStatusList: (String, Travis.BranchStatus) -> (String, List BuildStatus)
toBuildStatusList (repositoryName, {branches, commits}) = 
  (repositoryName, List.map2 combineAsBuildStatus branches commits)

combineAsBuildStatus: Travis.BranchBuild -> Travis.Commit -> BuildStatus
combineAsBuildStatus { state } { branch } = { state = state, branch = branch }

refreshBuilds : Configuration -> Effects Action 
refreshBuilds { apiKey, repositories } =
  let 
      repositoryTasks repository = Travis.getBranchBuildStatus apiKey repository
  in
     List.map repositoryTasks repositories 
        |> Task.sequence
        |> Task.map (NewBuildStatus << Util.sequence)
        |> Effects.task

flipAppMode: AppMode -> AppMode
flipAppMode mode = case mode of 
  Monitoring -> Config
  Config -> Monitoring

view: Signal.Address Action -> Model -> Html
view actionAddress model =
  let
     configMarkup = case model.mode of
       Config -> configPanel model.configPanel actionAddress 
       _ -> []
  in
     Html.div [] [
       Html.button [class "config-button", onClick actionAddress FlipConfigMode] [],
       Html.div [] configMarkup,
       buildRadiatorListing model.buildStatus
       ] 

buildRadiatorListing: List RepositoryStatus -> Html
buildRadiatorListing statuses =
  let
      asBuildListing repoStatus = Html.li [class "repository-item"] (buildRepositoryListing repoStatus)
  in
     Html.ul [class "repository-listing"] (List.map asBuildListing statuses)

buildRepositoryListing: RepositoryStatus -> List Html
buildRepositoryListing (repositoryName, buildStatuses) =
  let 
      headerItem = Html.li [class "repository-heading"] [Html.text repositoryName]
      singleton x = [x]
  in 
     List.take 5 buildStatuses
     |> List.map asListItem
     |> (::) headerItem
     |> Html.ul [class "branch-list"]
     |> singleton

asListItem: BuildStatus -> Html
asListItem s = Html.li [class ("branch " ++ s.state)] (branchElems s) 

branchElems: BuildStatus -> List Html
branchElems { branch } = [
    (Html.span [class "branch-name"] [Html.text branch])
  ]

configPanel: Configuration -> Signal.Address Action -> List Html
configPanel { repositories, apiKey } actionAddress = 
  let
      apiKeyValue = Maybe.withDefault "" apiKey
      repository = String.join "\n" repositories
  in 
     [Html.div [class "config-panel"] [
       Html.label [for "slug-field"] [Html.text "Repository slugs (one per line):"],
       Html.textarea [id "repository-field", value repository, rows 5, Html.Events.on "input" Html.Events.targetValue (Signal.message actionAddress << UpdateRepositoryField << String.split "\n")] [],
       Html.label [for "api-key-field"] [Html.text "Private Travis API key:"],
       Html.input [id "api-key-field", value apiKeyValue, Html.Events.on "input" Html.Events.targetValue (Signal.message actionAddress << UpdateApiKeyField)] [],
       Html.button [onClick actionAddress SaveConfiguration] [ Html.text "Save" ]
       ]]
