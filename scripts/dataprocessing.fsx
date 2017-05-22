
let naturalNumbers = Seq.unfold (fun i -> Some (i, i+1)) 1



type FileChunk = { Id : int; Start : int; Length : int}


let chunkFile chunkLength fileLength = 
    let rec loop pid start =
        seq {
            if fileLength <= start then ()
            else
                let length = if start + chunkLength <= fileLength
                             then chunkLength
                             else fileLength - start
                yield {Id=pid; Start=start; Length=length}
                yield! loop (pid+1) (start+length)
        }
    loop 0 0


let chunkFile' chunkSize fileSize =
    Seq.unfold (function  
                | _, start when fileSize <= start             
                    -> None
                | p, start when start + chunkSize <= fileSize 
                    -> Some ((p,start,start+chunkSize-1), (p+1,start+chunkSize))
                | p, start                                    
                    -> Some ((p,start,fileSize-1), (p+1,start+chunkSize))) (0,0)


                