let release = (2, 1, 1)

type RequiredUpdate =
    | Major
    | Minor
    | Build
    | NoUpdate

let updateCheck (rMaj, rMin, rBld) = function
    | (cMaj, _, _) when rMaj > cMaj                                     -> Major
    | (cMaj, cMin, _) when rMaj = cMaj && rMin > cMin                   -> Minor
    | (cMaj, cMin, cBld) when rMaj = cMaj && rMin = cMin && rMin > cBld -> Build
    | _                                                                 -> NoUpdate



let releaseUpdate = updateCheck release

let (|NoUpdate|_|) (rVer, cVer) =
    if rVer <= cVer then Some ()
    else None

let (|Major|Minor|Build|NoUpdate|) = function
    | NoUpdate                               -> NoUpdate
    | (rMaj,_,_),(cMaj,_,_) when rMaj > cMaj -> Major
    | (_,rMin,_),(_,cMin,_) when rMin > cMin -> Minor
    | _                                      -> Build

let updateCheck' rVer cVer = 
    match (rVer, cVer) with
    | Major    -> "Major update"
    | Minor    -> "Minor update"
    | Build    -> "Build update"
    | NoUpdate -> "No update"
    








#r "../packages/FsCheck/lib/net452/FsCheck"
open FsCheck

let isolateUserMajor x y = (x, x, x), (y, x, x)
let isolateUserMinor x y = (x, x, x), (x, y, x)
let isolateUserBuild x y = (x, x, x), (x, x, y)
let sameVersions x y z = (x, y, z), (x, y, z)

type UpdateRequiredProperties =
    static member ``Any major version larger than the release should match NoUpdate`` (major : PositiveInt) =
        match isolateUserMajor 0 major.Get with
        | NoUpdate -> true
        | _        -> false

    static member ``Any minor version larger than the release should match NoUpdate`` (minor : PositiveInt) =
        match ((0,0,0), (0, minor.Get,0)) with
        | NoUpdate -> true
        | _        -> false

    static member ``Any build version larger than the release should match NoUpdate`` (build : PositiveInt) =
        match ((0,0,0), (0,0, build.Get)) with
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