module RadiatorUpdate(update, refreshBuilds) where

import String
import Task
import Effects exposing (Effects)

import Travis
import RadiatorModel as Model exposing (..)
import Util


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
     RefreshBuilds ->
       (model, refreshBuilds model.configuration)

     NewBuildStatus (Just builds) ->
       (refreshModelBuildState builds model, Effects.none)

     NewBuildStatus Nothing -> (model, Effects.none)

     FlipConfigMode ->
       ({ model | mode = (flipAppMode model.mode) }, Effects.none)

     UpdateRepositoryField repository ->
       let cfg = model.configPanel
           configView = { cfg | repositorySlug = repository }
       in ({ model | configPanel = configView }, Effects.none)

     AddRepository ->
       let currentConfig = model.configuration
           cfgPanel = model.configPanel
           updatedRepositories = List.append model.configuration.repositories [model.configPanel.repositorySlug]
           updatedModel = ({ model | configuration = { currentConfig | repositories = updatedRepositories }, configPanel = { cfgPanel | repositorySlug = "" } })
       in (updatedModel, refreshBuilds updatedModel.configuration) 

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
       in ({ model | configPanel = configView }, Effects.none)

     SaveApiKey -> 
       updateConfig (\cfg -> {cfg | apiKey = (Just model.configPanel.apiKeyValue)}) model

     SaveConfiguration -> ({ model | configuration = model.configuration, 
        mode = Monitoring }, (refreshBuilds model.configuration))


updateConfig: (Configuration -> Configuration) -> Model -> (Model, Effects Action)
updateConfig f model =
  let cfg = model.configuration
      updatedModel = { model | configuration = f cfg }
  in (updatedModel, refreshBuilds updatedModel.configuration)


refreshModelBuildState: List (String, Travis.BranchStatus) -> Model -> Model 
refreshModelBuildState updatedBranchStatuses model =
  let radiatorStatuses = List.concatMap (toRadiatorStatusList << toBuildStatusList) updatedBranchStatuses
  in { model | buildStatus = radiatorStatuses }


toRadiatorStatusList: (String, List BuildStatus) -> List RadiatorStatus
toRadiatorStatusList (repository, branchBuildStatuses) =
  let nonPassed = List.filter (\build -> build.state /= "passed") (List.take 5 branchBuildStatuses)
  in case nonPassed of
    [] -> [RadiatorStatus repository Nothing "passed"]
    xs -> List.map (\build -> RadiatorStatus repository (Just build.branch) build.state) nonPassed


toBuildStatusList: (String, Travis.BranchStatus) -> (String, List BuildStatus)
toBuildStatusList (repositoryName, {branches, commits}) = 
  (repositoryName, List.map2 combineAsBuildStatus branches commits)


combineAsBuildStatus: Travis.BranchBuild -> Travis.Commit -> BuildStatus
combineAsBuildStatus { state } { branch } = { state = state, branch = branch }


refreshBuilds : Configuration -> Effects Action 
refreshBuilds { apiKey, repositories } =
  let repositoryTasks repository = Travis.getBranchBuildStatus apiKey repository
  in List.map repositoryTasks repositories
        |> Task.sequence
        |> Task.map (NewBuildStatus << Util.sequence)
        |> Effects.task


flipAppMode: AppMode -> AppMode
flipAppMode mode = case mode of 
  Monitoring -> Config
  Config -> Monitoring
