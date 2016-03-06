module RadiatorUpdate(update, refreshBuilds) where

import String
import Task
import Effects exposing (Effects)

import Travis
import RadiatorModel as Model exposing (..)
import Util
import Debug


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
     RefreshBuilds -> (model, (refreshBuilds model.configuration))
     NewBuildStatus (Just builds) -> ((refreshModelBuildState builds model), Effects.none)
     NewBuildStatus Nothing -> (model, Effects.none)
     FlipConfigMode -> ({ model | mode = (flipAppMode model.mode) }, Effects.none)
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
       let currentConfig = model.configuration
           newRepositories = List.filter (\r -> r /= repository) currentConfig.repositories
           updatedConfig = { currentConfig | repositories = newRepositories }
           updatedModel = { model | configuration = updatedConfig }
       in (updatedModel, refreshBuilds updatedConfig)
     UpdateApiKeyField key ->
       let cfg = model.configPanel
           configView = { cfg | apiKey = key }
       in ({ model | configPanel = configView }, Effects.none)
     SaveApiKey -> 
       let cfg = model.configuration
           cfgPanel = model.configPanel
           updatedCfg = {cfg | apiKey = (Just cfgPanel.apiKey)}
           updatedModel = {model | configuration = updatedCfg}
       in (updatedModel, refreshBuilds updatedModel.configuration)
     SaveConfiguration -> ({ model | configuration = model.configuration, 
        mode = Monitoring }, (refreshBuilds model.configuration))

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

