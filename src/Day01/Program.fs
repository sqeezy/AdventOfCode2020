// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let rec pairs l =
    match l with
    | [] | [_] -> []
    | h :: t ->
        [for x in t do
            yield h,x
         yield! pairs t]

let splitAtLinebreak (s : string) =
    s.Split([|'\n'|])

let partOne =
    input 
    |> splitAtLinebreak 
    |> Array.map (int)
    |> List.ofArray
    |> pairs
    |> List.map (fun (a,b) -> (a,b), a + b)
    |> List.filter (fun (_, sum) -> sum = 2020)
    |> List.map (fun ((a,b), _) -> a * b)

[<EntryPoint>]
let main argv =

    printfn "%A" (partOne)
    0 // return an integer exit code