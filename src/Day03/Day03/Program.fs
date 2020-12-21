open System
open Day03
open Day03.Input

type FieldType = Tree | Free
type Position = int * int
type Forest = Map<Position,FieldType>
let startPosition = 0,0
let stepSize = 3,1

let splitAtLinebreak (s : string) = s.Split([|'\n'|])

let fieldType = function
    |'#' -> Tree
    | _ -> Free

let lines = input |> splitAtLinebreak

let map =  lines
            |> Array.map (fun s -> s.ToCharArray()|> Array.map fieldType |> Array.indexed)
            |> Array.indexed
            |> Array.collect (fun (y, xs) -> xs |> Array.map (fun (x, fieldType) -> ((x,y), fieldType)))
            |> Map.ofArray

let width = lines.[0].Length
let height = lines.Length

let treesHit (right,down) =
    let ys = [0..down..height-1]
    let xs = [0..(ys.Length-1)] |> List.map (fun y -> y * right % width)

    let positions = List.zip xs ys

    let trees = positions |> List.map (fun p -> map.Item p) |> List.choose (function Tree -> Some Tree | _ -> None)

    trees.Length
let partTwo = [(1,1); (3,1); (5,1); (7,1); (1,2)]
                |> List.map treesHit
                |> List.fold (*) 1

[<EntryPoint>]
let main argv =
    printfn $"Part One: {treesHit (3,1)}"
    printfn $"Part Two: {partTwo}"
    0