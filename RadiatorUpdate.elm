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

