namespace Koak

module Parser = 
    type Expr = 
        | Double of float
        | Integer of int
        | Char of char
        | String of string
        | Boolean of bool
        | Variable of string
        | Binary of string * Expr * Expr
        | Unary of string * Expr
        | Call of string * Expr array
        | If of Expr * Expr * Expr
        | For of string * Expr * Expr * Expr option * Expr
    
    type Proto = 
        | Prototype of string * (string * string) array * string
        | BinaryPrototype of string * (string * string) array * int * string
    
    type Node = 
        | Def of Proto * Expr
        | Extern of Proto
        | TopExpr of Expr
    
    type Result<'a> = 
        | Success of 'a
        | Failure of string
    
    val parse : Lexer.Token list -> Result<Node list>
