module Radiator.Update exposing (update, refreshBuilds)

import String
import Result
import Task
import Http
import Platform.Cmd exposing (Cmd)
import Radiator.Ports as Ports

import Travis
import Radiator.Model as Model exposing (..)
import Util

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
     RefreshBuilds ->
       (model, refreshBuilds model.configuration)

     NewBuildStatus (Ok build) ->
       (refreshModelBuildState build model, Cmd.none)

     NewBuildStatus (Err _) -> (model, Cmd.none)

     FlipConfigMode ->
       ({ model | mode = (flipAppMode model.mode) }, Cmd.none)

     UpdateRepositoryField repository ->
       let cfg = model.configPanel
           configView = { cfg | repositorySlug = repository }
       in ({ model | configPanel = configView }, Cmd.none)

     AddRepository ->
       let currentConfig = model.configuration
           cfgPanel = model.configPanel
           updatedRepositories = List.append model.configuration.repositories [model.configPanel.repositorySlug]
           updatedModel = ({ model | configuration = { currentConfig | repositories = updatedRepositories }, configPanel = { cfgPanel | repositorySlug = "" } })
       in (updatedModel, updateConfigurationCmd updatedModel.configuration)

     RemoveRepository repository ->
       let newRepositories = List.filter (\r -> r /= repository) model.configuration.repositories
       in updateConfig (\cfg -> { cfg | repositories = newRepositories }) model

     TogglePrivateTravis usePrivateTravis ->
       let newApiKey = if usePrivateTravis 
                         then Just model.configPanel.apiKeyValue
                         else Nothing
       in updateConfig (\cfg -> { cfg | apiKey = newApiKey }) model

     UpdateApiKeyField key ->
       let cfg = model.configPanel
           configView = { cfg | apiKeyValue = key }
       in ({ model | configPanel = configView }, Cmd.none)

     SaveApiKey -> 
       updateConfig (\cfg -> {cfg | apiKey = (Just model.configPanel.apiKeyValue)}) model


updateConfig: (Configuration -> Configuration) -> Model -> (Model, Cmd Msg)
updateConfig f model =
  let cfg = model.configuration
      updatedModel = { model | configuration = f cfg }
  in (updatedModel, updateConfigurationCmd updatedModel.configuration)

updateConfigurationCmd: Model.Configuration -> Cmd Msg
updateConfigurationCmd config =
  Cmd.batch [Ports.saveConfiguration config, refreshBuilds config]

refreshModelBuildState: (String, Travis.BranchStatus) -> Model -> Model 
refreshModelBuildState newStatus model =
  let newBuildStatuses = toRadiatorStatusList <| toBuildStatusList newStatus
      radiatorStatuses = 
        List.filter (\x -> x.repository /= Tuple.first newStatus) model.buildStatus
         |> List.append newBuildStatuses
      updatedBuildStatus = List.concatMap (\r -> List.filter (\s -> s.repository == r) radiatorStatuses) model.configuration.repositories
  in { model | buildStatus = updatedBuildStatus }


toRadiatorStatusList: (String, List BuildStatus) -> List RadiatorStatus
toRadiatorStatusList (repository, branchBuildStatuses) =
  let nonPassed = List.filter (\build -> build.state /= "passed") (List.take 5 branchBuildStatuses)
  in case nonPassed of
    [] -> [RadiatorStatus repository Nothing "passed"]
    xs -> List.map (\build -> RadiatorStatus repository (Just build.branch) build.state) nonPassed


toBuildStatusList: (String, Travis.BranchStatus) -> (String, List BuildStatus)
toBuildStatusList = Tuple.mapSecond sortCombineBuildData


sortCombineBuildData: Travis.BranchStatus -> List BuildStatus
sortCombineBuildData {branches, commits} =
  List.map2 combineAsBuildStatus branches commits
    |> List.sortWith compareBuildNumberDesc


combineAsBuildStatus: Travis.BranchBuild -> Travis.Commit -> BuildStatus
combineAsBuildStatus { state, number } { branch } = {
    state = state,
    branch = branch,
    buildNumber = Result.withDefault -1 (String.toInt number)
  }


refreshBuilds : Configuration -> Cmd Msg 
refreshBuilds { apiKey, repositories } =
  let repositoryTasks repository =
    Http.send NewBuildStatus (Travis.getBranchBuildStatus apiKey repository)
  in List.map repositoryTasks repositories
    |> Cmd.batch


flipAppMode: AppMode -> AppMode
flipAppMode mode = case mode of 
  Monitoring -> Config
  Config -> Monitoring

compareBuildNumberDesc: BuildStatus -> BuildStatus -> Order
compareBuildNumberDesc a b = case compare a.buildNumber b.buildNumber of
  LT -> GT
  EQ -> EQ
  GT -> LT
