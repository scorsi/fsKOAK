module Program

open Koak
open System
open System.IO

[<EntryPoint>]
let main argv = 
    if (match Lexer.lex (File.ReadAllText argv.[0]) with
        | Lexer.Success tokens -> 
//            printfn "%A" tokens
            match Parser.parse tokens with
            | Parser.Success nodes -> 
//                printfn "%A" nodes
                match CodeGenerator.codegen nodes with
                | CodeGenerator.Success _ -> 
//                    printfn "It works"
                    true
                | CodeGenerator.Failure msg -> 
                    printfn "Error: %s" msg
                    false
            | Parser.Failure msg -> 
                printfn "Error: %s" msg
                false
        | Lexer.Failure msg -> 
            printfn "Error: %s" msg
            false)
    then 0
    else 1
