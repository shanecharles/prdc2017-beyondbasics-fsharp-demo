(*
type Option<'t> =
    | Some of 't
    | None
*)

let showSome data = 
    match data with 
    | Some v -> printfn "The data is: %A" v
    | None   -> printfn "There is no data"

showSome (Some "Bob")
showSome None
showSome (Some 45)







type RequiredUpdate =
    | Major
    | Minor
    | Build
    | NoUpdate

let checkForUpdate (rMaj, rMin, rBld) userVer = 
    match userVer with
    | (uMaj, _, _) when rMaj > uMaj                                     -> Major
    | (uMaj, uMin, _) when rMaj = uMaj && rMin > uMin                   -> Minor
    | (uMaj, uMin, uBld) when rMaj = uMaj && rMin = uMin && rMin > uBld -> Build
    | _                                                                 -> NoUpdate

checkForUpdate (2,2,2) (1,1,1)









type Version = { Major : int; Minor : int; Build : int }
let v1 = {Major=2; Minor=2; Build=2} 
let v2 = {v1 with Build=3}
v1 <= v2


let (|NoUpdate|_|) (relVer, userVer) =
    if relVer <= userVer then Some ()
    else None



let (|Major|Minor|Build|NoUpdate|) (relVer, userVer) =
    match (relVer, userVer) with
    | NoUpdate                                   -> NoUpdate
    | {Major=rMaj},{Major=uMaj} when rMaj > uMaj -> Major
    | {Minor=rMin},{Minor=uMin} when rMin > uMin -> Minor
    | _                                          -> Build


let checkUpdate relVer userVer = 
    match (relVer, userVer) with
    | Major    -> "Major"
    | Minor    -> "Minor"
    | Build    -> "Build"
    | NoUpdate -> "NoUpdate"











#r "../packages/FsCheck/lib/net452/FsCheck"
open FsCheck

let genVer x y z = {Major=x; Minor=y; Build=z}

let isolateUserMajor x y = genVer x x x, genVer y x x
let isolateUserMinor x y = genVer x x x, genVer x y x
let isolateUserBuild x y = genVer x x x, genVer x x y
let sameVersions x y z = genVer x y z, genVer x y z

type UpdateRequiredProperties =
    static member ``Any major version larger than the release should match NoUpdate`` (major : PositiveInt) =
        match isolateUserMajor 0 major.Get with
        | NoUpdate -> true
        | _        -> false

    static member ``Any minor version larger than the release should match NoUpdate`` (minor : PositiveInt) =
        match isolateUserMinor 0 minor.Get with
        | NoUpdate -> true
        | _        -> false

    static member ``Any build version larger than the release should match NoUpdate`` (build : PositiveInt) =
        match isolateUserBuild 0 build.Get with
        | NoUpdate -> true
        | _        -> false

    static member ``Any major version less than release should match Major`` (major : PositiveInt) =
        match isolateUserMajor System.Int32.MaxValue major.Get with
        | Major -> true
        | _     -> false

    static member ``Any minor version less than release should match Minor`` (minor : PositiveInt) = 
        match isolateUserMinor System.Int32.MaxValue minor.Get with
        | Minor -> true
        | _     -> false

    static member ``Any build version less than release should match Build`` (build : PositiveInt) =
        match isolateUserBuild System.Int32.MaxValue build.Get with
        | Build -> true
        | _     -> false

    static member ``Any user version equaling the release should match NoUpdate`` (major : NonNegativeInt) (minor : NonNegativeInt) (build : NonNegativeInt) =
        match sameVersions major.Get minor.Get build.Get with
        | NoUpdate -> true
        | _        -> false

Check.QuickAll<UpdateRequiredProperties>()