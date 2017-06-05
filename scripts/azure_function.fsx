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

let (|NoUpdatePartial|_|) (rVer, uVer) =
    if rVer <= uVer then Some ()
    else None

let (|Major|Minor|Build|NoUpdate|) = function
    | NoUpdatePartial                            -> NoUpdate
    | {Major=rMaj},{Major=uMaj} when rMaj > uMaj -> Major
    | {Minor=rMin},{Minor=uMin} when rMin > uMin -> Minor
    | _                                          -> Build

let checkUpdateAvailable relVer userVer = 
    match relVer, userVer with
    | NoUpdate -> None 
    | Major    -> Some (createUpgrade "Major")
    | Minor    -> Some (createUpgrade "Minor")
    | Build    -> Some (createUpgrade "Build")

let unsafeToVersion (unsafe : string) =
    if String.IsNullOrEmpty(unsafe) then None
    else 
        unsafe.Split('.') |> Array.map (Int32.TryParse) 
        |> function
        | [|(true, m); (true, n); (true, b)|] -> Some {Major=m; Minor=n; Build=b}
        | _                                   -> None


let Run(req: HttpRequestMessage, log: TraceWriter) =
    async {
        let releaseVersion = getCurrentRelease () |> unsafeToVersion
        if releaseVersion.IsSome then
            let! data = req.Content.ReadAsStringAsync() |> Async.AwaitTask
            if not (String.IsNullOrEmpty(data)) then
                try 
                    return 
                      match JsonConvert.DeserializeObject<UnsafeVersion>(data) with
                      | null   -> req.CreateResponse(HttpStatusCode.BadRequest, sprintf "Invalid user version: %s" data)
                      | unsafe -> 
                          unsafe.Version |> unsafeToVersion 
                          |> function 
                          | None             -> req.CreateResponse(HttpStatusCode.BadRequest, sprintf "Invalid user version: %s" unsafe.Version)
                          | Some userVersion ->
                                checkUpdateAvailable releaseVersion.Value userVersion
                                |> function
                                | None   -> req.CreateResponse(HttpStatusCode.NotModified)
                                | Some u -> req.CreateResponse(HttpStatusCode.OK,u) 
                with
                | ex ->
                    log.Error(ex.Message)
                    return req.CreateResponse(HttpStatusCode.BadRequest, "Invalid version format.");
            else
                return req.CreateResponse(HttpStatusCode.BadRequest, "No data.");
        else
            return req.CreateResponse(HttpStatusCode.InternalServerError, "oops")
    } |> Async.RunSynchronously
