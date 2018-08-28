module Travis exposing (Branch, BranchesResponse, Build, baseUrl, decodeBranch, decodeBranchesResponse, decodeBuild, getAuthHeaders, getBranchBuildStatus, travisApiGet, travisHeaders)

import Http
import Json.Decode exposing (..)
import Task exposing (Task)
import Time exposing (second)


type alias BranchesResponse =
    { branches : List Branch
    }


type alias Branch =
    { name : String
    , defaultBranch : Bool
    , existsOnGithub : Bool
    , lastBuild : Build
    }


type alias Build =
    { id : Int
    , number : String
    , state : String
    , eventType : String
    }


decodeBranchesResponse : Decoder BranchesResponse
decodeBranchesResponse =
    map BranchesResponse (field "branches" (list decodeBranch))


decodeBranch : Decoder Branch
decodeBranch =
    map4 Branch (field "name" string) (field "default_branch" bool) (field "exists_on_github" bool) (field "last_build" decodeBuild)


decodeBuild : Decoder Build
decodeBuild =
    map4 Build (field "id" int) (field "number" string) (field "state" string) (field "event_type" string)


baseUrl : Maybe String -> String
baseUrl maybeKey =
    case maybeKey of
        Just _ ->
            "https://api.travis-ci.com"

        Nothing ->
            "https://api.travis-ci.org"


getBranchBuildStatus : Maybe String -> String -> Http.Request ( String, List Branch )
getBranchBuildStatus apiKey repositorySlug =
    let
        url =
            baseUrl apiKey ++ "/repo/" ++ Http.encodeUri repositorySlug ++ "/branches?exists_on_github=true&sort_by=default_branch,last_build:desc"

        decoder =
            map (\result -> ( repositorySlug, result.branches )) decodeBranchesResponse
    in
    travisApiGet apiKey decoder url


travisApiGet : Maybe String -> Decoder a -> String -> Http.Request a
travisApiGet apiKey decoder url =
    let
        request =
            { method = "GET", headers = travisHeaders apiKey, url = url, body = Http.emptyBody, expect = Http.expectJson decoder, timeout = Just (20 * second), withCredentials = False }
    in
    Http.request request


travisHeaders : Maybe String -> List Http.Header
travisHeaders apiKey =
    List.append [ Http.header "Travis-API-Version" "3" ] (getAuthHeaders apiKey)


getAuthHeaders : Maybe String -> List Http.Header
getAuthHeaders maybeKey =
    case maybeKey of
        Just apiKey ->
            [ Http.header "Authorization" ("token " ++ apiKey) ]

        Nothing ->
            []
