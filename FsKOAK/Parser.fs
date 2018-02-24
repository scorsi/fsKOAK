namespace Koak

open Lexer

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
    
    exception MissingParenthesis
    
    let binaryOperators : Map<string, int> = 
        Map.empty.Add("+", 100).Add("-", 100).Add("*", 110).Add("/", 110).Add("%", 110).Add("=", 10).Add("+=", 10)
           .Add("-=", 10).Add("*=", 10).Add("/=", 10).Add("%=", 10).Add("<<=", 10).Add(">>=", 10).Add("&=", 10)
           .Add("^=", 10).Add("|=", 10).Add("==", 70).Add("<=", 80).Add(">=", 80).Add("<", 80).Add(">", 80)
           .Add("||", 20).Add("&&", 30).Add("|", 40).Add("^", 50).Add("&", 60).Add("<<", 90).Add(">>", 90)
    let unaryOperators : List<string> = "+" :: "-" :: "!" :: "~" :: []
    
    let private parsePrototypeArguments = 
        let rec parsePrototypeArguments' nodes tokens = 
            if (List.length tokens) <= 0 then Failure ""
            else Success(nodes, tokens)
        parsePrototypeArguments'
    
    let private parsePrototype = 
        let rec parsePrototype' nodes tokens = 
            if (List.length tokens) <= 0 then Failure ""
            else 
                try 
                    let id = 
                        match tokens.[0] with
                        | Lexer.Identifier id -> id
                        | _ -> raise MissingParenthesis
                    if tokens.[1] <> Lexer.Any "(" then raise MissingParenthesis
                    let arguments = 
                        match parsePrototypeArguments nodes tokens.[2..] with
                        | Success(nNodes, nTokens) -> nNodes
                        | Failure msg -> raise MissingParenthesis
                    Success(nodes, tokens)
                with MissingParenthesis -> Failure "Missing Parenthesis"
        parsePrototype'
    
    let private parseExtern = 
        let rec parseExtern' nodes tokens = 
            if (List.length tokens) <= 0 then Failure ""
            else Success(nodes, tokens)
        parseExtern'
    
    let private parseDef = 
        let rec parseDef' nodes tokens = 
            if (List.length tokens) <= 0 then Failure ""
            else Success(nodes, tokens)
        parseDef'
    
    let rec private parsePrimary (tokens : Token list) = 
        match tokens.[0] with
        | Token.Integer v -> Success(Expr.Integer v, tokens.[1..])
        | Token.Double v -> Success(Expr.Double v, tokens.[1..])
        | Token.Boolean v -> Success(Expr.Boolean v, tokens.[1..])
        | Token.Char v -> Success(Expr.Char v, tokens.[1..])
        | Token.If -> 
            if (List.length tokens) <= 1 then Failure "Expected condition in conditional branch."
            else parseIf tokens.[1..]
        // forExpr
        | Token.Identifier id -> Success(Expr.Variable id, tokens.[1..])
        | Token.Any "(" -> 
            if (List.length tokens) <= 1 then Failure "Expected condition in conditional branch."
            else 
                match parseExpr tokens.[1..] with
                | Success(expr, tokens) -> 
                    if (List.length tokens) <= 0 || tokens.[0] <> Token.Any ")" then 
                        Failure "Expected ')' in parenthesis expression."
                    else Success(expr, tokens.[1..])
        | _ -> Failure "Unkown statement"
    
    and private parseIf (tokens : Token list) = 
        match parseExpr tokens with
        | Success(condition, tokens) -> 
            if (List.length tokens) < 2 || tokens.[0] <> Token.Then then Failure "Expected 'then' in conditional branch"
            else 
                let tokens = tokens.[1..]
                match parseExpr tokens with
                | Success(thenExpr, tokens) -> 
                    if (List.length tokens) < 2 || tokens.[0] <> Token.Else then 
                        Failure "Expected 'else' in conditional branch"
                    else 
                        let tokens = tokens.[1..]
                        match parseExpr tokens with
                        | Success(elseExpr, tokens) -> Success(Expr.If(condition, thenExpr, elseExpr), tokens)
                        | failure -> failure
                | failure -> failure
        | failure -> failure
    
    and private parseBinop exprPrec lhs (tokens : Token list) = 
        match tokens.[0] with
        | Token.Any op when Map.containsKey op binaryOperators -> 
            let tokenPrec = Map.find op binaryOperators
            if tokenPrec < exprPrec then Success(lhs, tokens)
            else if (List.length tokens) <= 1 then Failure "Invalide right operand"
            else 
                match parseUnary tokens.[1..] with
                | Success(rhs, (tokens : Token list)) -> 
                    if (List.length tokens) <= 0 then Success(Expr.Binary(op, lhs, rhs), tokens)
                    else 
                        match tokens.[0] with
                        | Token.Any op2 when Map.containsKey op2 binaryOperators -> 
                            let nextPrec = Map.find op2 binaryOperators
                            if tokenPrec < nextPrec then 
                                match parseBinop (tokenPrec + 1) rhs tokens with
                                | Success(rhs, tokens) -> Success(Expr.Binary(op, lhs, rhs), tokens)
                                | failure -> failure
                            else Success(Expr.Binary(op, lhs, rhs), tokens)
                        | _ -> Success(Expr.Binary(op, lhs, rhs), tokens)
                | failure -> failure
        | Token.Any op -> Failure(String.concat "" ("Unknown binary operator: " :: op :: []))
        | _ -> Success(lhs, tokens)
    
    and private parseExpr tokens = 
        match parseUnary tokens with
        | Success(lhs, tokens) -> 
            if (List.length tokens) > 0 then parseBinop 0 lhs tokens
            else Success(lhs, tokens)
        | failure -> failure
    
    and private parseUnary tokens = 
        match tokens.[0] with
        | Token.Any op when List.contains op unaryOperators -> 
            match parseUnary tokens.[1..] with
            | Success(unary, tokens) -> Success(Expr.Unary(op, unary), tokens)
            | failure -> failure
        | Token.Any op -> Failure(String.concat "" ("Unknown unary operator: " :: op :: []))
        | _ -> parsePrimary tokens
    
    let parse tokens = 
        let rec parse' (nodes : Node list) tokens = 
            if (List.length tokens) <= 0 then Success nodes
            else 
                match tokens.[0] with
                | Lexer.Def -> 
                    match parseDef nodes tokens.[1..] with
                    | Success(nodes, tokens) -> 
                        if (List.length tokens) <= 0 || tokens.[0] <> Token.EOS then Failure "Missing ';'"
                        else parse' nodes tokens.[1..]
                    | Failure msg -> Failure msg
                | Lexer.Extern -> 
                    match parseExtern nodes tokens.[1..] with
                    | Success(nodes, tokens) -> 
                        if (List.length tokens) <= 0 || tokens.[0] <> Token.EOS then Failure "Missing ';'"
                        else parse' nodes tokens.[1..]
                    | Failure msg -> Failure msg
                | _ -> 
                    match parseExpr tokens with
                    | Success(expr, tokens) -> 
                        if (List.length tokens) <= 0 || tokens.[0] <> Token.EOS then Failure "Missing ';'"
                        else parse' (List.append nodes ((Node.TopExpr expr) :: [])) tokens.[1..]
                    | Failure msg -> Failure msg
        parse' [] tokens
