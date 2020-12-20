open Day02.Input
open FParsec

let splitAtLinebreak (s : string) =
    s.Split([|'\n'|])
let inputLines = input |> splitAtLinebreak

type Entry = {A : int; B : int; Letter : char; Pw : string}

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let lineParser = pipe4 pint32
                        (skipString "-" >>.pint32)
                        (spaces >>. asciiLower .>> skipString ":" .>> spaces)
                        (manyChars asciiLower)
                        (fun min max c pw -> {A=min; B=max; Letter=c; Pw=pw})

let validatePartOne {A=min; B=max; Letter=c; Pw=pw} =
    let matches = pw.ToCharArray() |> Array.filter (fun ch -> c = ch) |> Array.length
    min <= matches && matches <= max

let validatePartTwo {A=index1; B=index2; Letter=c; Pw=pw} =
    let letters = pw.ToCharArray()
    let isMatch index =  letters |> Array.tryItem (index - 1) |> Option.map ((=) c)
    [|index1; index2|]
    |> Array.map isMatch
    |> Array.choose (function Some true -> Some true | _ -> None)
    |> Array.length
    |> (=) 1




let solve validate = Array.map (run lineParser)
                     >> Array.choose (
                        function
                        | Success(entry,_,_) -> Some entry
                        | _ -> None )
                     >> Array.filter validate
                     >> Array.length

let partOneSolve = solve validatePartOne
let partOne = inputLines |> partOneSolve

let partTwoSolve = solve validatePartTwo
let partTwo = inputLines |> partTwoSolve

[<EntryPoint>]
let main argv =
    printfn $"Part One:\t{partOne}"
    printfn $"Part Two:\t{partTwo}"
    0