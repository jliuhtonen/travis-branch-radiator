module RadiatorView(view) where

import String
import Html exposing (Html)
import Html.Attributes exposing (class, id, for, value, rows)
import Html.Events exposing (onClick)

import RadiatorModel exposing (..)
import Util

view: Signal.Address Action -> Model -> Html
view actionAddress model =
  let
     configMarkup = case model.mode of
       Config -> configPanel model.configuration model.configPanel actionAddress 
       _ -> []
  in
     Html.div [] [
       Html.button [class "config-button", onClick actionAddress FlipConfigMode] [],
       Html.div [] configMarkup,
       buildRadiatorListing model.buildStatus
       ] 

buildRadiatorListing: List RepositoryStatus -> Html
buildRadiatorListing statuses =
  let
      asBuildListing repoStatus = Html.li [class "repository-item"] (buildRepositoryListing repoStatus)
  in
     Html.ul [class "repository-listing"] (List.map asBuildListing statuses)

buildRepositoryListing: RepositoryStatus -> List Html
buildRepositoryListing (repositoryName, buildStatuses) =
  let 
      repoDisplayName = displayableRepoName repositoryName
      headerItem = Html.li [class "repository-heading"] [Html.text repoDisplayName]
  in 
     List.take 5 buildStatuses
     |> List.map asListItem
     |> (::) headerItem
     |> Html.ul [class "branch-list"]
     |> Util.singleton

asListItem: BuildStatus -> Html
asListItem s = Html.li [class ("branch " ++ s.state)] (branchElems s) 

branchElems: BuildStatus -> List Html
branchElems { branch } = [
    (Html.span [class "branch-name"] [Html.text branch])
  ]

configPanel: Configuration -> ConfigPanel -> Signal.Address Action -> List Html
configPanel { repositories, apiKey } { repositorySlug, apiKeyValue } actionAddress = 
  let
      usePrivateTravis = Util.isJust apiKey
      repositoryItems = List.map (repositoryItem actionAddress) repositories
  in 
     [Html.div [class "config-panel"] [
       Html.button [class "config-close-button",
         onClick actionAddress FlipConfigMode] [] ,
       Html.h2 [] [Html.text "Configuration"],
       Html.h3 [] [Html.text "Repositories"],
       Html.ul [class "config-repository-list"] repositoryItems,
       repositoryInput repositorySlug actionAddress,
       Html.h3 [] [Html.text "API"],
       usePrivateTravisInput usePrivateTravis actionAddress,
       apiKeyInput apiKeyValue actionAddress,
       attributions
       ]]

repositoryInput: String -> Signal.Address Action -> Html
repositoryInput repositorySlug address =
  Html.div [] [
    Html.label [for "add-repository"] [Html.text "Add a new repository"],
    Html.div [class "config-panel-control-row"] [
      Html.input [Html.Attributes.type' "text", id "add-repository", value repositorySlug, Html.Events.on "input" Html.Events.targetValue (Signal.message address << UpdateRepositoryField)] [] ,
      Html.button [onClick address AddRepository] [Html.text "Add"]
      ]]

usePrivateTravisInput: Bool -> Signal.Address Action -> Html
usePrivateTravisInput private actionAddress =
  Html.div [class "config-panel-control-row"] [
    Html.input [Html.Attributes.id "use-private-travis-checkbox", 
        Html.Attributes.type' "checkbox", 
        Html.Attributes.checked private,
        Html.Events.on "change" Html.Events.targetChecked (Signal.message actionAddress << TogglePrivateTravis)] [],
    Html.label [for "use-private-travis-checkbox"] [Html.text "Use private Travis"]
    ]

apiKeyInput: String -> Signal.Address Action -> Html
apiKeyInput apiKey actionAddress =
  Html.div [] [
    Html.label [for "api-key-field"] [Html.text "Private Travis API key:"],
    Html.div [class "config-panel-control-row"] [
      Html.input [Html.Attributes.type' "text", id "api-key-field", value apiKey, Html.Events.on "input" Html.Events.targetValue (Signal.message actionAddress << UpdateApiKeyField)] [],
      Html.button [onClick actionAddress SaveApiKey] [ Html.text "Set" ]
      ]]

repositoryItem: Signal.Address Action -> String -> Html
repositoryItem address repoName =
  let clickOptions = { preventDefault = True, stopPropagation = False }
  in Html.li [] [
    Html.span [] [Html.text repoName],
    Html.a [Html.Events.onClick address (RemoveRepository repoName), Html.Attributes.href "#"] [Html.img [class "remove-repository-icon", Html.Attributes.src "close-circular-button.svg"] []]
    ]

displayableRepoName: String -> String
displayableRepoName name =
  let nameParts = String.split "/" name
  in if List.length nameParts > 1
     then List.drop 1 nameParts
          |> List.head
          |> Maybe.withDefault name
     else name

attributions: Html
attributions =
  Html.div [class "attributions"] [
    Html.text "Icons made by ",
    Html.a [Html.Attributes.href "http://www.flaticon.com/authors/robin-kylander"] [Html.text "Robin Kylander"],
    Html.text " from www.flaticon.com are licensed by CC 3.0 BY"
    ]
