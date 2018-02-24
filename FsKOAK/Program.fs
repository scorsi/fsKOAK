module Program

open Koak
open System
open System.IO

[<EntryPoint>]
let main argv = 
    let ret = 
        match Lexer.lex (File.ReadAllText argv.[0]) with
        | Lexer.Success tokens -> 
            match Parser.parse tokens with
            | Parser.Success nodes -> 
                printfn "%A" nodes
                true
            | Parser.Failure msg -> 
                printfn "Error: %s" msg
                false
        | Lexer.Failure msg -> 
            printfn "Error: %s" msg
            false
    if ret = true then 0
    else 1
