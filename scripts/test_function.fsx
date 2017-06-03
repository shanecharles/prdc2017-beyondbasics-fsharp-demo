#r "System.Net.Http"
#r "../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

open System
open System.Text
open System.Net
open System.Net.Http
open Newtonsoft.Json

let client = new HttpClient ()

type UpdateResponse = { Url : string; UpgradeType : string }

type UpdateResult =
    | Update of UpdateResponse
    | NoUpdate
    | Error of string

let build = new StringContent ("""{"Version":"2.2.1"}""", Encoding.UTF8)
let minor = new StringContent ("""{"Version":"2.1.1"}""", Encoding.UTF8)
let major = new StringContent ("""{"Version":"1.1.1"}""", Encoding.UTF8)

let invalid = new StringContent ("""{"Version":"cat"}""", Encoding.UTF8)

let url = "https://prdc2017beyondbasics.azurewebsites.net/api/checkupdatebuilder"

let display = function
    | Update {UpgradeType="Major";Url=url} -> System.Diagnostics.Process.Start("open", url) |> ignore
    | Update {UpgradeType=ut}              -> printfn "Upgrade: %s" ut
    | NoUpdate                             -> printfn "There are no updates."
    | Error msg                            -> printfn "Error: %s" msg


let checkUpdate (url : string) (content : HttpContent) =
    async {
        let! result = client.PostAsync ( Uri(url), content) |> Async.AwaitTask
        let! content = result.Content.ReadAsStringAsync () |> Async.AwaitTask
        let result = match result.StatusCode with
                     | HttpStatusCode.OK          -> Update (JsonConvert.DeserializeObject<UpdateResponse>(content))
                     | HttpStatusCode.NotModified -> NoUpdate
                     | _                          -> Error content
        display result
    }

[build; minor; invalid]
    |> List.map (checkUpdate url)
    |> Async.Parallel
    |> Async.Ignore
    |> Async.Start

major |> checkUpdate url |> Async.Start
