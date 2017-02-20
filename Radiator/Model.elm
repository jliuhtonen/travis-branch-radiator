module Radiator.Model exposing (..)

import Travis
import Http as Http


type Msg = RefreshBuilds 
            | NewBuildStatus (Result Http.Error (String, Travis.BranchStatus))
            | FlipConfigMode 
            | UpdateRepositoryField String
            | AddRepository
            | RemoveRepository String
            | UpdateApiKeyField String 
            | TogglePrivateTravis Bool
            | SaveApiKey


type alias Model = {
  mode: AppMode,
  configuration: Configuration,
  configPanel: ConfigPanel,
  buildStatus : List RadiatorStatus
}

type alias RadiatorStatus = {
  repository: String,
  branch: Maybe String,
  state: String
}


type alias Configuration = {
  apiKey: Maybe String,
  repositories: List String
}


type alias ConfigPanel = {
  repositorySlug: String,
  apiKeyValue: String
}


type alias BuildStatus = {
  branch: String,
  state: String,
  buildNumber: Int
}


type AppMode = Monitoring | Config
