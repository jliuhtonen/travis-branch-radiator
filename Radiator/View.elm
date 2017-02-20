module Radiator.View exposing (view)

import String
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

import Radiator.Model exposing (..)
import Util


view: Model -> Html Msg
view model =
  let configMarkup = case model.mode of
    Config -> configPanel model.configuration model.configPanel 
    _ -> []
  in H.div [] [
       H.button [A.class "config-button", E.onClick FlipConfigMode] [],
       H.div [] configMarkup,
       buildRepositoryListing model.buildStatus
       ] 


buildRepositoryListing: List RadiatorStatus -> Html Msg
buildRepositoryListing statuses =
  let listElems = List.map asListItem statuses
  in H.ul [A.class "branch-list"] listElems


asListItem: RadiatorStatus -> Html Msg
asListItem s = H.li [A.class ("branch " ++ s.state)] (branchElems s) 


branchElems: RadiatorStatus -> List (Html Msg)
branchElems { repository, branch } = 
  let repoName = displayableRepoName repository 
      branchName = Maybe.withDefault repoName (Maybe.map ((++) (repoName ++ ": ")) branch)
  in [H.span [A.class "branch-name"] [H.text branchName]]


configPanel: Configuration -> ConfigPanel -> List (Html Msg)
configPanel { repositories, apiKey } { repositorySlug, apiKeyValue } = 
  let usePrivateTravis = Util.isJust apiKey
      repositoryItems = List.map repositoryItem repositories
  in [H.div [A.class "config-panel"] [
       H.button [A.class "config-close-button",
         E.onClick FlipConfigMode] [] ,
       H.h2 [] [H.text "Configuration"],
       H.section [] [
         H.h3 [] [H.text "Repositories"],
         H.ul [A.class "config-repository-list"] repositoryItems,
         repositoryInput repositorySlug
         ],
       H.section [] [
         H.h3 [] [H.text "API"],
         usePrivateTravisInput usePrivateTravis,
         apiKeyInput apiKeyValue
         ],
       attributions
       ]]


repositoryInput: String -> Html Msg
repositoryInput repositorySlug =
  H.div [] [
    H.label [A.for "add-repository"] [H.text "Add a new repository"],
    H.div [A.class "config-panel-control-row"] [
      H.input [A.type_ "text", A.id "add-repository", A.value repositorySlug, E.onInput UpdateRepositoryField] [] ,
      H.button [E.onClick AddRepository] [H.text "Add"]
      ]]


usePrivateTravisInput: Bool -> Html Msg
usePrivateTravisInput private =
  H.div [A.class "config-panel-control-row"] [
    H.input [A.id "use-private-travis-checkbox", 
        A.type_ "checkbox",
        A.checked private,
        E.onCheck TogglePrivateTravis] [],
    H.label [A.for "use-private-travis-checkbox"] [H.text "Use private Travis"]
    ]


apiKeyInput: String -> Html Msg
apiKeyInput apiKey =
  H.div [] [
    H.label [A.for "api-key-field"] [H.text "Private Travis API key:"],
    H.div [A.class "config-panel-control-row"] [
      H.input [A.type_ "text", A.id "api-key-field", A.value apiKey, E.onInput UpdateApiKeyField] [],
      H.button [E.onClick SaveApiKey] [ H.text "Set" ]
      ]]


repositoryItem: String -> Html Msg
repositoryItem repoName =
  let clickOptions = { preventDefault = True, stopPropagation = False }
  in H.li [] [
    H.span [] [H.text repoName],
    H.a [E.onClick (RemoveRepository repoName), A.href "#"] [H.img [A.class "remove-repository-icon", A.src "img/close-circular-button.svg"] []]
    ]


displayableRepoName: String -> String
displayableRepoName name =
  let nameParts = String.split "/" name
  in if List.length nameParts > 1
     then List.drop 1 nameParts
          |> List.head
          |> Maybe.withDefault name
     else name


attributions: Html Msg
attributions =
  H.div [A.class "attributions"] [
    H.text "Icons made by ",
    H.a [A.href "http://www.flaticon.com/authors/robin-kylander"] [H.text "Robin Kylander"],
    H.text " from www.flaticon.com are licensed by CC 3.0 BY"
    ]
