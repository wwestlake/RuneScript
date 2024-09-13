// File: Main.fs

open FParsec
open RuneScript
open Parser

[<EntryPoint>]
let main argv =
    printfn "Welcome to RuneScript!"
    let input = "if (x > 0) { x = x - 1; } else { x = 0; }"
    match run runescriptParser input with
    | Success(result, _, _) -> printfn "Parsed successfully: %A" result
    | Failure(errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg
    0 // return an integer exit code
