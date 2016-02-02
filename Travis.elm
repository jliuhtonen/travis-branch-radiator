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

baseUrl : String
baseUrl = "https://api.travis-ci.org/"

getBranchBuildStatus : String -> Task never (Maybe BranchStatus)
getBranchBuildStatus repositorySlug =
  travisApiGet decodeBranchStatus (baseUrl ++ "/repos/" ++ repositorySlug ++ "/branches")
    |> Task.toMaybe

travisApiGet : Decoder a -> String -> Task Http.Error a
travisApiGet decoder url =
  let request =
    { verb = "GET", headers = travisHeaders, url = url, body = Http.empty }
  in Http.send Http.defaultSettings request |> Http.fromJson decoder

travisHeaders : List (String, String)
travisHeaders = [("Accept", "application/vnd.travis-ci.2+json")]
