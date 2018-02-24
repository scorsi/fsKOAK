module Program

open Koak
open System
open System.IO

[<EntryPoint>]
let main argv = 
    match Lexer.lex (File.ReadAllText argv.[0]) with
    | Lexer.Success tokens -> 
        match Parser.parse tokens with
        | Parser.Success nodes -> printfn "%A" nodes
        | Parser.Failure msg -> printfn "Error: %s" msg
    | Lexer.Failure msg -> printfn "Error: %s" msg
    0
