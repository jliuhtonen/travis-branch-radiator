module RadiatorView(view) where

import String
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

import RadiatorModel exposing (..)
import Util


view: Signal.Address Action -> Model -> Html
view actionAddress model =
  let configMarkup = case model.mode of
    Config -> configPanel model.configuration model.configPanel actionAddress 
    _ -> []
  in H.div [] [
       H.button [A.class "config-button", E.onClick actionAddress FlipConfigMode] [],
       H.div [] configMarkup,
       buildRadiatorListing model.buildStatus
       ] 


buildRadiatorListing: List RepositoryStatus -> Html
buildRadiatorListing statuses =
  let asBuildListing repoStatus = H.li [A.class "repository-item"] (buildRepositoryListing repoStatus)
  in H.ul [A.class "repository-listing"] (List.map asBuildListing statuses)


buildRepositoryListing: RepositoryStatus -> List Html
buildRepositoryListing (repositoryName, buildStatuses) =
  let repoDisplayName = displayableRepoName repositoryName
      headerItem = H.li [A.class "repository-heading"] [H.text repoDisplayName]
  in List.take 5 buildStatuses
     |> List.map asListItem
     |> (::) headerItem
     |> H.ul [A.class "branch-list"]
     |> Util.singleton


asListItem: BuildStatus -> Html
asListItem s = H.li [A.class ("branch " ++ s.state)] (branchElems s) 


branchElems: BuildStatus -> List Html
branchElems { branch } = [
    (H.span [A.class "branch-name"] [H.text branch])
  ]


configPanel: Configuration -> ConfigPanel -> Signal.Address Action -> List Html
configPanel { repositories, apiKey } { repositorySlug, apiKeyValue } actionAddress = 
  let usePrivateTravis = Util.isJust apiKey
      repositoryItems = List.map (repositoryItem actionAddress) repositories
  in [H.div [A.class "config-panel"] [
       H.button [A.class "config-close-button",
         E.onClick actionAddress FlipConfigMode] [] ,
       H.h2 [] [H.text "Configuration"],
       H.section [] [
         H.h3 [] [H.text "Repositories"],
         H.ul [A.class "config-repository-list"] repositoryItems,
         repositoryInput repositorySlug actionAddress
         ],
       H.section [] [
         H.h3 [] [H.text "API"],
         usePrivateTravisInput usePrivateTravis actionAddress,
         apiKeyInput apiKeyValue actionAddress
         ],
       attributions
       ]]


repositoryInput: String -> Signal.Address Action -> Html
repositoryInput repositorySlug address =
  H.div [] [
    H.label [A.for "add-repository"] [H.text "Add a new repository"],
    H.div [A.class "config-panel-control-row"] [
      H.input [A.type' "text", A.id "add-repository", A.value repositorySlug, E.on "input" E.targetValue (Signal.message address << UpdateRepositoryField)] [] ,
      H.button [E.onClick address AddRepository] [H.text "Add"]
      ]]


usePrivateTravisInput: Bool -> Signal.Address Action -> Html
usePrivateTravisInput private actionAddress =
  H.div [A.class "config-panel-control-row"] [
    H.input [A.id "use-private-travis-checkbox", 
        A.type' "checkbox", 
        A.checked private,
        E.on "change" E.targetChecked (Signal.message actionAddress << TogglePrivateTravis)] [],
    H.label [A.for "use-private-travis-checkbox"] [H.text "Use private Travis"]
    ]


apiKeyInput: String -> Signal.Address Action -> Html
apiKeyInput apiKey actionAddress =
  H.div [] [
    H.label [A.for "api-key-field"] [H.text "Private Travis API key:"],
    H.div [A.class "config-panel-control-row"] [
      H.input [A.type' "text", A.id "api-key-field", A.value apiKey, E.on "input" E.targetValue (Signal.message actionAddress << UpdateApiKeyField)] [],
      H.button [E.onClick actionAddress SaveApiKey] [ H.text "Set" ]
      ]]


repositoryItem: Signal.Address Action -> String -> Html
repositoryItem address repoName =
  let clickOptions = { preventDefault = True, stopPropagation = False }
  in H.li [] [
    H.span [] [H.text repoName],
    H.a [E.onClick address (RemoveRepository repoName), A.href "#"] [H.img [A.class "remove-repository-icon", A.src "close-circular-button.svg"] []]
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
  H.div [A.class "attributions"] [
    H.text "Icons made by ",
    H.a [A.href "http://www.flaticon.com/authors/robin-kylander"] [H.text "Robin Kylander"],
    H.text " from www.flaticon.com are licensed by CC 3.0 BY"
    ]
