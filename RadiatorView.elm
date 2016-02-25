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
       Config -> configPanel model.configPanel actionAddress 
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

configPanel: Configuration -> Signal.Address Action -> List Html
configPanel { repositories, apiKey } actionAddress = 
  let
      apiKeyValue = Maybe.withDefault "" apiKey
      repository = String.join "\n" repositories
  in 
     [Html.div [class "config-panel"] [
       Html.label [for "slug-field"] [Html.text "Repository slugs (one per line):"],
       Html.textarea [id "repository-field", value repository, rows 5, Html.Events.on "input" Html.Events.targetValue (Signal.message actionAddress << UpdateRepositoryField << String.split "\n")] [],
       Html.label [for "api-key-field"] [Html.text "Private Travis API key:"],
       Html.input [id "api-key-field", value apiKeyValue, Html.Events.on "input" Html.Events.targetValue (Signal.message actionAddress << UpdateApiKeyField)] [],
       Html.button [onClick actionAddress SaveConfiguration] [ Html.text "Save" ]
       ]]

displayableRepoName: String -> String
displayableRepoName name =
  let nameParts = String.split "/" name
  in if List.length nameParts > 1
     then List.drop 1 nameParts
          |> List.head
          |> Maybe.withDefault name
     else name
