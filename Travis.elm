module Travis where

import Json.Decode exposing (..)
import Http
import Task exposing (Task)

type alias BranchStatus = {
  branches : List BranchBuild,
  commits : List Commit
}

decodeBranchStatus: Decoder BranchStatus
decodeBranchStatus = object2 BranchStatus ("branches" := (list decodeBranchBuild)) ("commits" := (list decodeCommit))

type alias BranchBuild = {
  id : Int,
  commitId : Int,
  state: String,
  number: String
}

decodeBranchBuild: Decoder BranchBuild
decodeBranchBuild = object4 BranchBuild ("id" := int) ("commit_id" := int) ("state" := string) ("number" := string)

type alias Commit = {
  id: Int,
  branch: String,
  committerName: String,
  committerEmail: String,
  message: String
}

decodeCommit: Decoder Commit
decodeCommit = object5 Commit ("id" := int) ("branch" := string) ("committer_name" := string) ("committer_email" := string) ("message" := string)

baseUrl : Maybe String -> String
baseUrl maybeKey = case maybeKey of
  Just _  -> "https://api.travis-ci.com"
  Nothing -> "https://api.travis-ci.org"

getBranchBuildStatus : Maybe String -> String -> Task never (Maybe (String, BranchStatus))
getBranchBuildStatus apiKey repositorySlug =
  let url = (baseUrl apiKey) ++ "/repos/" ++ repositorySlug ++ "/branches"
  in travisApiGet apiKey decodeBranchStatus url 
     |> Task.map (\xs -> (repositorySlug, xs))
     |> Task.toMaybe

travisApiGet : Maybe String -> Decoder a -> String -> Task Http.Error a
travisApiGet apiKey decoder url =
  let request =
    { verb = "GET", headers = travisHeaders apiKey, url = url, body = Http.empty }
  in Http.send Http.defaultSettings request |> Http.fromJson decoder

travisHeaders : Maybe String -> List (String, String)
travisHeaders apiKey = List.append [("Accept", "application/vnd.travis-ci.2+json")] (getAuthHeaders apiKey)

getAuthHeaders: Maybe String -> List (String, String)
getAuthHeaders maybeKey = case maybeKey of
  Just apiKey -> [("Authorization", "token " ++ apiKey)]
  Nothing  -> []
