let sayHello name = printfn "Hello %s" name

let sayHello' = printfn "Hello %s"

let add x y = x + y

let increment = add 1
let decrement = add -1


let seven = 3 |> add 4


let seekKey keys key = 
    printfn "Creating index"
    let keyset = Set.ofSeq keys
    printfn "Searching for key: %d" key
    if keyset |> Set.contains key 
    then printfn "Key %d was found!" key
    else printfn "Key %d was not found." key


let seekKey' keys =
    printfn "Creating index"
    let keyset = Set.ofSeq keys
    (fun key -> 
        printfn "Searching for key: %d" key
        if keyset |> Set.contains key 
        then printfn "Key %d was found!" key
        else printfn "Key %d was not found." key)



type Cattitude =
    | Lazy
    | Grumpy
    | Sarcastic
    | Indifferent

type Cat = { name : string; mood : Cattitude }





