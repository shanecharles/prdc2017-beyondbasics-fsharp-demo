let fib n =
    let rec loop x y c = 
        if c = n then y
        else loop y (x+y) (c+1)
    loop 0L 1L 1

fib 100

let fibSeq n =
    let rec loop x y = seq {
        yield y
        yield! loop y (x+y)
    }
    loop 0L 1L |> Seq.take n

fibSeq 10 |> Seq.toArray

// Seq.unfold (
let fibSeq' n =
    Seq.unfold (fun (x,y) -> Some (y, (y, x+y))) (0L,1L) 
    |> Seq.take n


fibSeq' 10 |> Seq.toArray

(fibSeq 100 |> Seq.toArray) = (fibSeq' 100 |> Seq.toArray)