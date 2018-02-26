namespace Koak

open System

module Lexer = 
    type Token = 
        | EOS
        | Def
        | Extern
        | Binary
        | Unary
        | If
        | Then
        | Else
        | For
        | In
        | LeftParenthesis
        | RightParenthesis
        | Identifier of string
        | Integer of int
        | Double of float
        | Char of char
        | String of string
        | Boolean of bool
        | Any of string
    
    type Result<'a> = 
        | Success of 'a
        | Failure of string
    
    let private lexIdentifier = 
        let rec lexIdentifier' (buf : char list) tokens (str : string) = 
            let findToken id = 
                match id with
                | "def" -> Def
                | "extern" -> Extern
                | "binary" -> Binary
                | "unary" -> Unary
                | "if" -> If
                | "then" -> Then
                | "else" -> Else
                | "for" -> For
                | "in" -> In
                | _ -> Identifier id
            if (String.length str) <= 0 then 
                let id = String.Concat(Array.ofList buf)
                ((List.append tokens ((findToken id) :: [])), str)
            else 
                match str.[0] with
                | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' -> 
                    lexIdentifier' (List.append buf (c :: [])) tokens str.[1..]
                | _ -> 
                    let id = String.Concat(Array.ofList buf)
                    ((List.append tokens ((findToken id) :: [])), str)
        lexIdentifier'
    
    let private lexDouble = 
        let rec lexDouble' (buf : char list) tokens (str : string) = 
            if (String.length str) <= 0 then 
                let number = String.Concat(Array.ofList buf) |> double
                ((List.append tokens ((Double number) :: [])), str)
            else 
                match str.[0] with
                | c when c >= '0' && c <= '9' -> lexDouble' (List.append buf (c :: [])) tokens str.[1..]
                | _ -> 
                    let number = String.Concat(Array.ofList buf) |> double
                    ((List.append tokens ((Double number) :: [])), str)
        lexDouble'
    
    let private lexInteger = 
        let rec lexInteger' (buf : char list) tokens (str : string) = 
            if (String.length str) <= 0 then 
                let number = String.Concat(Array.ofList buf) |> int
                ((List.append tokens ((Integer number) :: [])), str)
            else 
                match str.[0] with
                | c when c >= '0' && c <= '9' -> lexInteger' (List.append buf (c :: [])) tokens str.[1..]
                | '.' -> lexDouble (List.append buf ('.' :: [])) tokens str.[1..]
                | _ -> 
                    let number = String.Concat(Array.ofList buf) |> int
                    ((List.append tokens ((Integer number) :: [])), str)
        lexInteger'
    
    let private lexAny = 
        let rec lexAny' (buf : char list) tokens (str : string) = 
            if (String.length str) <= 0 then 
                let v = String.Concat(Array.ofList buf)
                ((List.append tokens ((Any v) :: [])), str)
            else 
                match str.[0] with
                | c when c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n' && c <> ';' && (c < 'a' || c > 'z') 
                         && (c < 'A' || c > 'Z') && (c < '0' || c > '9') && c <> '`' -> 
                    lexAny' (List.append buf (c :: [])) tokens str.[1..]
                | _ -> 
                    let v = String.Concat(Array.ofList buf)
                    ((List.append tokens ((Any v) :: [])), str)
        lexAny'
    
    let private lexMagicAny = 
        let rec lexMagicAny' (buf : char list) tokens (str : string) = 
            if (String.length str) <= 0 then 
                let v = String.Concat(Array.ofList buf)
                ((List.append tokens ((Any v) :: [])), str)
            else 
                match str.[0] with
                | c when c <> '`' -> lexMagicAny' (List.append buf (c :: [])) tokens str.[1..]
                | _ -> 
                    let v = String.Concat(Array.ofList (List.append buf ('`' :: [])))
                    ((List.append tokens ((Any v) :: [])), str)
        lexMagicAny'
    
    let lex str = 
        let rec lex' tokens str = 
            if String.length str <= 0 then Success tokens
            else 
                match str.[0] with
                // EMPTY LINE
                | ' ' | '\t' | '\r' | '\n' -> lex' tokens str.[1..]
                // IDENTIFIER
                | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' -> 
                    let (tokens, str) = lexIdentifier (c :: []) tokens str.[1..]
                    lex' tokens str
                // INTEGER
                | c when c >= '0' && c <= '9' -> 
                    let (tokens, str) = lexInteger (c :: []) tokens str.[1..]
                    lex' tokens str
                // DOUBLE
                | '.' -> 
                    let (tokens, str) = lexDouble ('0' :: '.' :: []) tokens str.[1..]
                    lex' tokens str
                // SEMICOLON = End Of Statement
                | ';' -> lex' (List.append tokens (Token.EOS :: [])) str.[1..]
                // PARENTHESIS
                | '(' -> lex' (List.append tokens (Token.LeftParenthesis :: [])) str.[1..]
                | ')' -> lex' (List.append tokens (Token.RightParenthesis :: [])) str.[1..]
                // ANY
                | '`' -> 
                    let (tokens, str) = lexMagicAny ('`' :: []) tokens str.[1..]
                    if (String.length str) <= 0 || str.[0] <> '`' then Failure "Magic binary operator must be closed"
                    else lex' tokens str.[1..]
                | ''' -> 
                    if (String.length str) <= 2 then 
                        let (tokens, str) = lexAny (''' :: []) tokens str.[1..]
                        lex' tokens str
                    else if str.[2] <> ''' then 
                        let (tokens, str) = lexAny (''' :: []) tokens str.[1..]
                        lex' tokens str
                    else lex' (List.append tokens ((Token.Char str.[1]) :: [])) str.[3..]
                | c -> 
                    let (tokens, str) = lexAny (c :: []) tokens str.[1..]
                    lex' tokens str
        lex' [] str
