#r "System.Net.Http"
#r "System.Web.Http"
#r "System.Net.Http.Formatting"
#r "System.Configuration"
#r "../packages/Microsoft.Azure.WebJobs/lib/net45/Microsoft.Azure.WebJobs.Host.dll"
#r "../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

open System
open System.Net
open System.Net.Http
open Microsoft.Azure.WebJobs.Host
open System.Configuration
open Newtonsoft.Json

type BuilderResult<'a> =
    | Success of 'a
    | NoUpdateRequired
    | InvalidReleaseFormat of string
    | InvalidDataFromClient of string
    | Fail of string

type UpdateBuilder () =
    member x.Bind (m : BuilderResult<'a>, f : 'a -> BuilderResult<'b>) =
        match m with 
        | Success v               -> f v
        | NoUpdateRequired        -> NoUpdateRequired
        | Fail s                  -> Fail s
        | InvalidDataFromClient s -> InvalidDataFromClient s
        | InvalidReleaseFormat s  -> InvalidReleaseFormat s

    member x.Return(v) = Success v
    member x.ReturnFrom(m : BuilderResult<'a>) = m

[<AllowNullLiteral>]
type UnsafeVersion () = 
    let mutable version = ""
    member x.Version 
        with get() = version
        and set(v) = version <- v

type Version = { Major : int; Minor : int; Build : int }

type UpgradeResult (upgradeType, url) = 
    member x.UpgradeType = upgradeType
    member x.URL = url

let getCurrentRelease () = ConfigurationManager.AppSettings.["RELEASE_VERSION"]

let createUpgrade (upgradeType : string) =
    let url = sprintf "%s%s_upgrade.jpg" ConfigurationManager.AppSettings.["UPGRADE_BASE_URL"] (upgradeType.ToLower())
    UpgradeResult (upgradeType, url)

let (|NoUpdate|_|) (rVer, uVer) =
    if rVer <= uVer then Some ()
    else None

let (|Major|Minor|Build|NoUpdate|) = function
    | NoUpdate                                   -> NoUpdate
    | {Major=rMaj},{Major=uMaj} when rMaj > uMaj -> Major
    | {Minor=rMin},{Minor=uMin} when rMin > uMin -> Minor
    | _                                          -> Build

let checkUpdateAvailable releaseVersion userVersion =
    match releaseVersion, userVersion with
    | NoUpdate -> NoUpdateRequired 
    | Major    -> Success (createUpgrade "Major")
    | Minor    -> Success (createUpgrade "Minor")
    | Build    -> Success (createUpgrade "Build")

let unsafeToVersion builderResultError (unsafe : string) =
    if String.IsNullOrEmpty(unsafe) then builderResultError "Version data is missing."
    else 
        unsafe.Split('.') |> Array.map (Int32.TryParse) 
          |> function 
          | [|(true, m); (true, n); (true, b)|] -> Success {Major=m; Minor=n; Build=b}
          | _                                   -> builderResultError (sprintf "Invalid Version Format: %s" unsafe)

let deserializeUserVersion data =
    try
        match JsonConvert.DeserializeObject<UnsafeVersion>(data) with
        | null   -> InvalidDataFromClient "No data could be retrieved."
        | unsafe -> unsafe.Version |> unsafeToVersion InvalidDataFromClient
    with
    | ex -> InvalidDataFromClient (ex.Message)

let getContentData (content : HttpContent) =
    content.ReadAsStringAsync () |> Async.AwaitTask 
    |> Async.RunSynchronously

let run (req: HttpRequestMessage, log: TraceWriter) =
    let update = UpdateBuilder ()
    update {
        let! relVer = getCurrentRelease () |> unsafeToVersion InvalidReleaseFormat
        let! userVer = req.Content |> getContentData |> deserializeUserVersion
        return! checkUpdateAvailable relVer userVer }
    |> function
    | Success result          -> req.CreateResponse(HttpStatusCode.OK, result)  
    | NoUpdateRequired        -> req.CreateResponse(HttpStatusCode.NotModified)
    | InvalidDataFromClient s -> req.CreateResponse(HttpStatusCode.BadRequest, s)
    | Fail s                  -> log.Error (sprintf "Catastrophic Failure: %s" s)
                                 req.CreateResponse(HttpStatusCode.InternalServerError, "Something has gone horribly wrong.")
    | InvalidReleaseFormat s  -> log.Error (sprintf "Invalid Release Format: %s" s)
                                 req.CreateResponse(HttpStatusCode.InternalServerError,"Our bad.")