namespace Koak

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
    
    val lex : string -> Result<Token list>
