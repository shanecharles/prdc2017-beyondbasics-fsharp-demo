open System

let release = (2, 2, 2)

let (|RGB|) (color : System.Drawing.Color) =
    (color.R, color.G, color.B)

let printRGB = function
    | RGB(r,g,b) -> printfn "Red: %d, Green: %d, Blue: %d" r g b

let color1 = System.Drawing.Color.FromArgb(100,200,148)
printRGB color1

let averageRGBs c1 c2 = 
    let avg x y = (int x + int y) / 2 |> byte
    match c1, c2 with
    | RGB(r,g,b), RGB(r',g',b') -> (avg r r', avg g g', avg b b')

let color2 = System.Drawing.Color.FromArgb(255,200,100,152)



type Version = { Major : int; Minor : int; Build : int }

let (|FileVersion|) (x : int, y : int, z : int) = {Major = x; Minor = y; Build = z}

let printVersion = function FileVersion v -> printfn "%A" v

printVersion release

let (|NoUpdate|_|) (relVer, userVer) =
    if relVer <= userVer then Some ()
    else None

let (|Major|Minor|Build|NoUpdate|) = function
    | NoUpdate                                   -> NoUpdate
    | {Major=rMaj},{Major=uMaj} when rMaj > uMaj -> Major
    | {Minor=rMin},{Minor=uMin} when rMin > uMin -> Minor
    | _                                          -> Build





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