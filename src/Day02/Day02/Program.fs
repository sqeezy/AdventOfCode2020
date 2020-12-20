open System
open Day02.Input
open FParsec

let splitAtLinebreak (s : string) =
    s.Split([|'\n'|])
let inputLines = input |> splitAtLinebreak

type Entry = {Min : int; Max : int; Letter : char; Pw : string}

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let lineParser = pipe4 pint32
                        (skipString "-" >>.pint32)
                        (spaces >>. asciiLower .>> skipString ":" .>> spaces)
                        (manyChars asciiLower)
                        (fun min max c pw -> {Min=min; Max=max; Letter=c; Pw=pw})

let validate {Min=min; Max=max; Letter=c; Pw=pw} =
    let matches = pw.ToCharArray() |> Array.filter (fun ch -> c = ch) |> Array.length
    min <= matches && matches <= max

let partOne = inputLines
              |> Array.map (run lineParser)
              |> Array.choose (function
                 | Success(entry,_,_) -> Some entry
                 | _ -> None )
              |> Array.filter validate
              |> Array.length

test lineParser "3-7 f: fffffftfffhfff"

[<EntryPoint>]
let main argv =
    printf $"{partOne}"
    0