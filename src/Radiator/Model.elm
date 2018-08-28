module Radiator.Model exposing (AppMode(..), BuildStatus, ConfigPanel, Configuration, Model, Msg(..), RadiatorStatus)

import Http as Http
import Travis


type Msg
    = RefreshBuilds
    | NewBuildStatus (Result Http.Error ( String, List Travis.Branch ))
    | FlipConfigMode
    | UpdateRepositoryField String
    | AddRepository
    | RemoveRepository String
    | UpdateApiKeyField String
    | TogglePrivateTravis Bool
    | SaveApiKey


type alias Model =
    { mode : AppMode
    , configuration : Configuration
    , configPanel : ConfigPanel
    , buildStatus : List RadiatorStatus
    }


type alias RadiatorStatus =
    { repository : String
    , branch : Maybe String
    , state : String
    }


type alias Configuration =
    { apiKey : Maybe String
    , repositories : List String
    }


type alias ConfigPanel =
    { repositorySlug : String
    , apiKeyValue : String
    }


type alias BuildStatus =
    { branch : String
    , state : String
    , buildNumber : Int
    }


type AppMode
    = Monitoring
    | Config
