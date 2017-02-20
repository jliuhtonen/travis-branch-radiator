module Travis exposing (..)

import Json.Decode exposing (..)
import Http
import Task exposing (Task)
import Time exposing (second)


type alias BranchStatus = {
  branches : List BranchBuild,
  commits : List Commit
}


type alias BranchBuild = {
  id : Int,
  commitId : Int,
  state: String,
  number: String
}


type alias Commit = {
  id: Int,
  branch: String,
  committerName: String,
  committerEmail: String,
  message: String
}


decodeBranchStatus: Decoder BranchStatus
decodeBranchStatus = map2 BranchStatus (field "branches" (list decodeBranchBuild)) (field "commits" (list decodeCommit))


decodeBranchBuild: Decoder BranchBuild
decodeBranchBuild = map4 BranchBuild (field "id" int) (field "commit_id" int) (field "state" string) (field "number" string)


decodeCommit: Decoder Commit
decodeCommit = map5 Commit (field "id" int) (field "branch" string) (field "committer_name" string) (field "committer_email" string) (field "message" string)


baseUrl : Maybe String -> String
baseUrl maybeKey = case maybeKey of
  Just _  -> "https://api.travis-ci.com"
  Nothing -> "https://api.travis-ci.org"


getBranchBuildStatus : Maybe String -> String -> Http.Request (String, BranchStatus)
getBranchBuildStatus apiKey repositorySlug =
  let url = (baseUrl apiKey) ++ "/repos/" ++ repositorySlug ++ "/branches"
      decoder = map (\result -> (repositorySlug, result)) decodeBranchStatus
  in travisApiGet apiKey decoder url

travisApiGet : Maybe String -> Decoder a -> String -> Http.Request a
travisApiGet apiKey decoder url =
  let request =
    { method = "GET", headers = travisHeaders apiKey, url = url, body = Http.emptyBody, expect = Http.expectJson decoder, timeout = Just (20 * second), withCredentials = False }
  in Http.request request


travisHeaders : Maybe String -> List Http.Header
travisHeaders apiKey = List.append [Http.header "Accept" "application/vnd.travis-ci.2+json"] (getAuthHeaders apiKey)


getAuthHeaders: Maybe String -> List Http.Header
getAuthHeaders maybeKey = case maybeKey of
  Just apiKey -> [Http.header "Authorization" ("token " ++ apiKey)]
  Nothing  -> []
