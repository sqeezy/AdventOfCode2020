open System
open Day01.Input

let rec pairs l =
    match l with
    | [] | [_] -> []
    | h :: t ->
        [for x in t do
            yield h,x
         yield! pairs t]

let rec trips l =
    match l with
    | [] | [_] -> []
    | h :: t ->
        let others = pairs t
        [for a,b in others do
             yield a,b,h
         yield! trips t]

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

let partTwo =
    input
    |> splitAtLinebreak
    |> Array.map (int)
    |> List.ofArray
    |> trips
    |> List.map (fun (a,b,c) -> (a,b,c), a + b + c)
    |> List.filter (fun (_, sum) -> sum = 2020)
    |> List.map (fun ((a,b,c), _) -> a * b * c)

[<EntryPoint>]
let main argv =
    printfn "%A" (partOne)
    printfn "%A" (partTwo)
    0