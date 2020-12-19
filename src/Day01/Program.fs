// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let splitAtLinebreak (s : string) =
    s.Split([|'\n'|])

let integerInput =
    input 
    |> splitAtLinebreak 
    |> Array.map (int) 

[<EntryPoint>]
let main argv =

    printfn "%A" "result"
    0 // return an integer exit code