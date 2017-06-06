let helloWorld = "hello World"

let helloWorld' () = "hello world"


let sayHello name = printfn "Hello %s." name

sayHello "Bob"

let yellHello (name : string) = printfn "HELLO %s!" (name.ToUpper())
//let yellHello' name = printfn "HELLO %s!" (name.ToUpper())

yellHello "Bob"

let add x y = x + y

add 4 3

let increaseByFour = add 4

increaseByFour 3





let fibSeq = 
    let rec loop (x, y) = seq {
            yield y
            yield! loop (y, (x+y))
        }
    loop (0L, 1L)


fibSeq |> Seq.skip 40
  |> Seq.take 4
  |> Seq.map string
  |> Seq.toList







// nulls

[<AllowNullLiteral>]
type Cat (name : string, remainingLives : int) = 
    member x.Name = name
    member x.RemainingLives = remainingLives

let mutable cat2 : Cat = null
cat2 |> isNull





#r "../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json
let nullCat = JsonConvert.DeserializeObject<Cat>("")
