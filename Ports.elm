port module Ports exposing (saveConfiguration)


import RadiatorModel as Model


port saveConfiguration: Model.Configuration -> Cmd msg
