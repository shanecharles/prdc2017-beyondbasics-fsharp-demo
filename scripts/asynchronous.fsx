
open System.IO
open System.Net

let getUrlContentSizeAsync (url : string) = async {
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

sites |> Array.map getUrlContentSizeAsync
  |> Async.Parallel 
  |> Async.RunSynchronously 
  |> Array.sortByDescending snd
