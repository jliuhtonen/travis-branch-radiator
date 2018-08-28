port module Radiator.Ports exposing (saveConfiguration)

import Radiator.Model as Model


port saveConfiguration : Model.Configuration -> Cmd msg
