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

type Version = {
    Major : int
    Minor : int
    Build : int
}

type Upgrade (upgradeType, url) = 
    member x.UpgradeType = upgradeType
    member x.URL = url

let createUpgrade baseURL upgradeType upgradeFile =
    Upgrade (upgradeType, sprintf "%s%s" baseURL upgradeFile)

let (|NoUpdate|_|) (rVer, uVer) =
    if rVer <= uVer then Some ()
    else None

let (|Major|Minor|Build|NoUpdate|) = function
    | NoUpdate                                   -> NoUpdate
    | {Major=rMaj},{Major=uMaj} when rMaj > uMaj -> Major
    | {Minor=rMin},{Minor=uMin} when rMin > uMin -> Minor
    | _                                          -> Build

let parseVersion (version : string) =
    match version.Split([|'.'|]) with
    | [|maj; mnr; bld|] -> { Major = Convert.ToInt32(maj)
                             Minor = Convert.ToInt32(mnr)
                             Build = Convert.ToInt32(bld) }
    | _                 -> failwith "Bad release version format"


let Run(req: HttpRequestMessage, log: TraceWriter) =
    async {
        let upgrade = createUpgrade ConfigurationManager.AppSettings.["UPGRADE_BASE_URL"]
        let releaseVersion = ConfigurationManager.AppSettings.["RELEASE_VERSION"]
                             |> parseVersion

        let! data = req.Content.ReadAsStringAsync() |> Async.AwaitTask

        if not (String.IsNullOrEmpty(data)) then
            try 
                let userVersion = JsonConvert.DeserializeObject<Version>(data)
                let update = match releaseVersion, userVersion with
                             | NoUpdate -> None 
                             | Major    -> Some (upgrade "Major" "major_upgrade.jpg")
                             | Minor    -> Some (upgrade "Minor" "minor_upgrade.jpg")
                             | Build    -> Some (upgrade "Build" "build_upgrade.jpg")

                return match update with
                       | None   -> req.CreateResponse(HttpStatusCode.NotModified)
                       | Some u -> req.CreateResponse(HttpStatusCode.OK,u) 
            with
            | ex ->
                log.Error(ex.Message)
                return req.CreateResponse(HttpStatusCode.BadRequest, "Invalid version format.");
        else
            return req.CreateResponse(HttpStatusCode.BadRequest, "No data.");
    } |> Async.RunSynchronously
