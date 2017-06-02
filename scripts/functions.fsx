let sayHello name = printfn "Hello %s." name

sayHello "Bob"

let yellHello (name : string) = printfn "HELLO %s!" (name.ToUpper())
//let yellHello' name = printfn "HELLO %s!" (name.ToUpper())

yellHello "Bob"

let add x y = x + y

let increment = add 1

increment 7

let decrement = add -1

decrement 7

let incrementDecrement = increment >> decrement

incrementDecrement 7



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
