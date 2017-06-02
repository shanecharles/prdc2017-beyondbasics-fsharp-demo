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


let fibonacci n =
    let rec loop x y c = 
        if c = n then y
        else loop y (x+y) (c+1)
    loop 0L 1L 1

fibonacci 100

let fibSeq =
    let rec loop x y = seq {
        yield y
        yield! loop y (x+y)
    }
    loop 0L 1L

fibSeq |> Seq.take 10 |> Seq.toArray

let fibSeq' = Seq.unfold (fun (x,y) -> Some (y, (y, x+y))) (0L,1L) 

fibSeq' |> Seq.take 10 |> Seq.toArray

fibSeq |> Seq.take 10 
  |> Seq.map (fun x -> x * x * x)
  |> Seq.filter (fun x -> x % 2L = 0L)
  |> Seq.toArray


let getFibArray n = Seq.take n >> Seq.toArray

(fibSeq |> getFibArray 100) = (fibSeq' |> getFibArray 100)
