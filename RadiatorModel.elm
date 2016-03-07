module RadiatorModel where

import Travis

type Action = RefreshBuilds 
            | NewBuildStatus (Maybe (List (String, Travis.BranchStatus)))
            | FlipConfigMode 
            | UpdateRepositoryField String
            | AddRepository
            | RemoveRepository String
            | UpdateApiKeyField String 
            | TogglePrivateTravis Bool
            | SaveApiKey
            | SaveConfiguration

type alias Model = {
  mode: AppMode,
  configuration: Configuration,
  configPanel: ConfigPanel,
  buildStatus : List RepositoryStatus
}

type alias RepositoryStatus = (String, List BuildStatus)

type alias Configuration = {
  apiKey: Maybe String,
  repositories: List String
}

type alias ConfigPanel = {
  repositorySlug: String,
  apiKeyValue: String
}

type alias BuildStatus = {
  branch : String,
  state : String
}

type AppMode = Monitoring | Config
