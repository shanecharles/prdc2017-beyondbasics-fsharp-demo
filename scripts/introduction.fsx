let sayHello name = printfn "Hello %s." name

sayHello "Bob"

let yellHello (name : string) = printfn "HELLO %s!" (name.ToUpper())
//let yellHello' name = printfn "HELLO %s!" (name.ToUpper())

yellHello "Bob"

let add x y = x + y

add 4 3

let increment = add 1

increment 7


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
