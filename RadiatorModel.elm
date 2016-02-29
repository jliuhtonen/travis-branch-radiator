module RadiatorModel where

import Travis

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
