open System
open FParsec

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
let fieldParsers = allowedFields |> List.map parseField
let fieldParser = choice fieldParsers

let passport = sepBy fieldParser (pchar ' '<|> newline)

test passport @"pid:646234624 cid:234
hgt:185cm"

test passport @"pid:646234624 cid:234
hgt:185cm

pid:646234624 cid:234
hgt:185cm"

let manyPassports = sepBy passport (skipNewline .>> skipNewline)

test manyPassports @"pid:646234624 cid:234
hgt:185cm

pid:646234624 cid:234
hgt:185cm"

[<EntryPoint>]
let main argv =
    0 // return an integer exit code