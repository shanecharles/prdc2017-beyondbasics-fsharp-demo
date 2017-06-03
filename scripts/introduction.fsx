let sayHello name = printfn "Hello %s." name

sayHello "Bob"

let yellHello (name : string) = printfn "HELLO %s!" (name.ToUpper())
//let yellHello' name = printfn "HELLO %s!" (name.ToUpper())

yellHello "Bob"

let add x y = x + y

add 4 3

let increment = add 1

increment 7




// Partial Application doesn't mean execute some of the function.
let datapoints = [|1 .. 400000|]
let searchKeys = [1000; 50; -1; 1000000]

#time

let seekKey keys key = 
    printfn "Creating index"
    let keyset = Set.ofSeq keys
    printfn "Searching for key: %d" key
    Set.contains key keyset


seekKey datapoints 100

List.map (seekKey datapoints) searchKeys


let seekKey' keys =
    printfn "Creating index"
    let keyset = Set.ofSeq keys
    (fun key -> 
        printfn "Searching for key: %d" key
        Set.contains key keyset)


seekKey' datapoints 1000

List.map (seekKey' datapoints) searchKeys


// Pattern Matching

open System

let showSome data = 
    match data with 
    | Some d -> printfn "The data is: %A" d
    | None   -> printfn "There is no data"

showSome (Some "Bob")
showSome None
showSome (Some 45)

let greetAstronaut (name : string) =
    match name.ToUpper() with
    | "NEIL" -> printfn "All hail the first person to step on the moon!"
    | "DAVE" -> printfn "I'm sorry %s, I'm afraid I can't do that." name
    | _      -> printfn "Hi %s!" name


greetAstronaut "dave"
greetAstronaut "Neil"


let release = (2, 2, 2)

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


let testUpdate userVer =
    let check = checkForUpdate release
    printfn "Release: %A" release
    printfn "User: %A" userVer
    userVer |> check |> printfn "Update: %A" 

testUpdate (1,1,1)
testUpdate (2,1,1)
testUpdate (2,2,1)
testUpdate (2,2,2)
