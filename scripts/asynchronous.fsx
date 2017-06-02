
open System.IO
open System.Net

let getUrlContentSizeAsync (url : string) = 
    async {
        let request   = WebRequest.Create (url)
        use! response = request.AsyncGetResponse ()
        use stream    = response.GetResponseStream ()   
        use reader    = new StreamReader(stream)
        let! content  = reader.ReadToEndAsync () |> Async.AwaitTask
        
        return (url, content.Length)
    }

getUrlContentSizeAsync "http://techandwings.ca" |> Async.RunSynchronously

let sites = [| "http://techandwings.ca"
               "http://google.ca"
               "http://cnn.com"
               "http://apple.com"
               "http://foxnews.com" |]

let asyncSites = sites |> Array.map getUrlContentSizeAsync

// #time



// Serially asynchronous
asyncSites |> Array.map Async.RunSynchronously |> Array.sortByDescending snd




// Get the lengths in parallel
asyncSites |> Async.Parallel |> Async.RunSynchronously |> Array.sortByDescending snd



let getUrlsContentSizeAsync urls =
    async {
        for url in urls do
            let! urlSize = getUrlContentSizeAsync url
            printfn "URL: %A" urlSize
    }

let moreSites = [ "http://techandwings.ca" 
                  "http://www.cnn.com"
                  "http://www.foxnews.com"
                  "http://msnbc.com"
                  "http://google.ca"
                  "http://nhl.com"
                  "http://mlb.com"
                  "http://cfl.ca" ]


open System.Threading
let cancellationSource = new CancellationTokenSource()
Async.Start (moreSites |> getUrlsContentSizeAsync, cancellationSource.Token)
cancellationSource.Cancel()
