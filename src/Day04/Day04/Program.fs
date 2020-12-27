open System
open Day04.Input
open FParsec

let rec splitOn elem arr =
    let index = List.tryFindIndex elem arr
    match index with
    | Some i ->
        let (a, rem) = arr |> List.splitAt (i+1)
        let skipElem = List.take (a.Length-1) a
        skipElem :: splitOn elem rem
    | None -> []


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let whitespaceTextChars = " \r\n "
let whitespaceChar = skipAnyOf whitespaceTextChars
test whitespaceChar @"
"

let parseField key = pstring key .>> skipChar ':' .>> many(noneOf whitespaceTextChars)

let allowedFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid"]
let neededFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
let fieldParsers = allowedFields |> List.map parseField
let fieldParser = choice fieldParsers

let passport = sepBy fieldParser (pchar ' '<|> newline)

test passport @"pid:646234624 cid:234
hgt:185cm"

test passport @"pid:646234624 cid:234
hgt:185cm

pid:646234624 cid:234
hgt:185cm"

let manyPassports = sepBy passport (skipNewline .>> skipNewline .>> skipNewline)

test manyPassports @"pid:646234624 cid:234
hgt:185cm

pid:646234624 cid:234
hgt:185cm"

let input = @"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

let passportFields = input.Split '\n'
                        |> List.ofArray
                        |> splitOn ((=)"")
                        |> List.map (String.concat " ")
                        |> List.map (run passport)
                        |> List.choose (function Success(list, _, _) -> Some list | _ -> None)
                        |> List.map (fun list -> neededFields |> List.forall (fun need -> List.contains need list))
                        |> List.choose (function true -> Some 0 | _ -> None)
                        |> List.length

[<EntryPoint>]
let main argv =
    0 // return an integer exit code