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
        | Statements of Expr list
    
    type Proto = 
        | Prototype of string * (string * string option) list * string option
        | BinaryPrototype of string * (string * string option) list * int * string option
    
    type Node = 
        | Def of Proto * Expr
        | Extern of Proto
        | TopExpr of Expr
    
    type Result<'a> = 
        | Success of 'a
        | Failure of string
    
    exception MissingParenthesis
    
    type OperatorAssoc = 
        | Right
        | Left
    
    type Operator = 
        { precedence : int
          custom : bool
          assoc : OperatorAssoc }
    
    let defaultOperator = 
        { precedence = 0
          custom = false
          assoc = OperatorAssoc.Left }
    
    let binaryOperators : Map<string, Operator> = 
        Map.empty.Add("+", { defaultOperator with precedence = 100 })
           .Add("-", { defaultOperator with precedence = 100 }).Add("*", { defaultOperator with precedence = 110 })
           .Add("/", { defaultOperator with precedence = 110 }).Add("%", { defaultOperator with precedence = 110 })
           .Add("==", { defaultOperator with precedence = 70 }).Add("<=", { defaultOperator with precedence = 80 })
           .Add(">=", { defaultOperator with precedence = 80 }).Add("<", { defaultOperator with precedence = 80 })
           .Add(">", { defaultOperator with precedence = 80 }).Add("||", { defaultOperator with precedence = 20 })
           .Add("&&", { defaultOperator with precedence = 30 }).Add("|", { defaultOperator with precedence = 40 })
           .Add("^", { defaultOperator with precedence = 50 }).Add("&", { defaultOperator with precedence = 60 })
           .Add("<<", { defaultOperator with precedence = 90 }).Add(">>", { defaultOperator with precedence = 90 })
           .Add("=", 
                { defaultOperator with precedence = 10
                                       assoc = Right }).Add("+=", 
                                                            { defaultOperator with precedence = 10
                                                                                   assoc = Right })
           .Add("-=", 
                { defaultOperator with precedence = 10
                                       assoc = Right }).Add("*=", 
                                                            { defaultOperator with precedence = 10
                                                                                   assoc = Right })
           .Add("/=", 
                { defaultOperator with precedence = 10
                                       assoc = Right }).Add("%=", 
                                                            { defaultOperator with precedence = 10
                                                                                   assoc = Right })
           .Add("<<=", 
                { defaultOperator with precedence = 10
                                       assoc = Right }).Add(">>=", 
                                                            { defaultOperator with precedence = 10
                                                                                   assoc = Right })
           .Add("&=", 
                { defaultOperator with precedence = 10
                                       assoc = Right }).Add("^=", 
                                                            { defaultOperator with precedence = 10
                                                                                   assoc = Right })
           .Add("|=", 
                { defaultOperator with precedence = 10
                                       assoc = Right })
    
    let unaryOperators : Map<string, Operator> = 
        Map.empty.Add("+", defaultOperator).Add("-", defaultOperator).Add("~", defaultOperator)
           .Add("!", defaultOperator)
    
    let rec private parsePrimary (tokens : Token list) = 
        match tokens.[0] with
        | Token.Integer v -> Success(Expr.Integer v, tokens.[1..])
        | Token.Double v -> Success(Expr.Double v, tokens.[1..])
        | Token.Boolean v -> Success(Expr.Boolean v, tokens.[1..])
        | Token.Char v -> Success(Expr.Char v, tokens.[1..])
        | Token.If -> 
            if (List.length tokens) <= 1 then Failure "Expected condition in conditional branch"
            else parseIf tokens.[1..]
        | Token.For -> 
            if (List.length tokens) <= 1 then Failure "Expected assignment expression in for loop"
            else parseFor tokens.[1..]
        | Token.Identifier id -> Success(Expr.Variable id, tokens.[1..])
        | Token.Any "(" -> 
            if (List.length tokens) <= 1 then Failure "Expected condition in conditional branch"
            else 
                match parseStatements tokens.[1..] with
                | Success(statements, tokens) -> 
                    if (List.length tokens) <= 0 || tokens.[0] <> Token.Any ")" then 
                        Failure "Expected ')' in parenthesis expression"
                    else Success(statements, tokens.[1..])
                | failure -> failure
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
    
    and private parseFor (tokens : Token list) = 
        match tokens.[0] with
        | Token.Identifier id -> 
            if (List.length tokens) < 2 || tokens.[1] <> Token.Any "=" then 
                Failure "Expected '=' in assignment expression in for loop"
            else if (List.length tokens) < 3 then Failure "Expected assignment expression in for loop"
            else 
                match parseExpr tokens.[2..] with
                | Success(assignExpr, tokens) -> 
                    if (List.length tokens) < 1 || tokens.[0] <> Token.Any "," then 
                        Failure "Expected ',' between assignment expression and condition expression in for loop"
                    else if (List.length tokens) < 2 then Failure "Expected condition expression in for loop"
                    else 
                        match parseExpr tokens.[1..] with
                        | Success(conditionExpr, tokens) -> 
                            if (List.length tokens) < 1 then 
                                Failure "Expected assignment-step expression or body expression in for loop"
                            else 
                                match tokens.[0] with
                                | Token.Any "," -> 
                                    if (List.length tokens) < 2 then 
                                        Failure "Expected assignment-step expression in for loop"
                                    else 
                                        match parseExpr tokens.[1..] with
                                        | Success(stepExpr, tokens) -> 
                                            if (List.length tokens) < 1 || tokens.[0] <> Token.In then 
                                                Failure "Expected in before body expression in for loop"
                                            else if (List.length tokens) < 2 then 
                                                Failure "Expected body expression in for loop"
                                            else 
                                                match parseExpr tokens.[1..] with
                                                | Success(bodyExpr, tokens) -> 
                                                    Success
                                                        (Expr.For
                                                             (id, assignExpr, conditionExpr, Some(stepExpr), bodyExpr), 
                                                         tokens)
                                                | failure -> failure
                                        | failure -> failure
                                | Token.In -> 
                                    if (List.length tokens) < 2 then Failure "Expected body expression in for loop"
                                    else 
                                        match parseExpr tokens.[1..] with
                                        | Success(bodyExpr, tokens) -> 
                                            Success(Expr.For(id, assignExpr, conditionExpr, None, bodyExpr), tokens)
                                        | failure -> failure
                                | _ -> Failure "Expected in before body expression in for loop"
                        | failure -> failure
                | failure -> failure
        | _ -> Failure "Expected identifier for assignment in for loop"
    
    and private parseBinop exprPrec lhs (tokens : Token list) = 
        match tokens.[0] with
        | Token.Any op when Map.containsKey op binaryOperators -> 
            let tokenPrec = (Map.find op binaryOperators).precedence
            if tokenPrec < exprPrec then Success(lhs, tokens)
            else if (List.length tokens) <= 1 then Failure "Invalide right operand"
            else 
                match parseUnary tokens.[1..] with
                | Success(rhs, (tokens : Token list)) -> 
                    if (List.length tokens) <= 0 then Success(Expr.Binary(op, lhs, rhs), tokens)
                    else 
                        match tokens.[0] with
                        | Token.Any op2 when Map.containsKey op2 binaryOperators -> 
                            let nextPrec = (Map.find op2 binaryOperators).precedence
                            if tokenPrec < nextPrec then 
                                match parseBinop (tokenPrec + 1) rhs tokens with
                                | Success(rhs, tokens) -> Success(Expr.Binary(op, lhs, rhs), tokens)
                                | failure -> failure
                            else Success(Expr.Binary(op, lhs, rhs), tokens)
                        | _ -> Success(Expr.Binary(op, lhs, rhs), tokens)
                | failure -> failure
        | Token.Any op when op <> "(" && op <> ")" && op <> "," && op <> ":" -> 
            Failure(String.concat "" ("Unknown binary operator: " :: op :: []))
        | _ -> Success(lhs, tokens)
    
    and private parseExpr tokens = 
        match parseUnary tokens with
        | Success(lhs, tokens) -> 
            if (List.length tokens) > 0 then parseBinop 0 lhs tokens
            else Success(lhs, tokens)
        | failure -> failure
    
    and private parseUnary tokens = 
        match tokens.[0] with
        | Token.Any op when Map.containsKey op unaryOperators -> 
            match parseUnary tokens.[1..] with
            | Success(unary, tokens) -> Success(Expr.Unary(op, unary), tokens)
            | failure -> failure
        | Token.Any op when op <> "(" && op <> ")" && op <> "," && op <> ":" -> 
            Failure(String.concat "" ("Unknown unary operator: " :: op :: []))
        | _ -> parsePrimary tokens
    
    and private parseStatements tokens = 
        let rec parseStatements' list tokens = 
            match parseExpr tokens with
            | Success(expr, tokens) -> 
                let list = List.append list (expr :: [])
                if (List.length tokens) > 0 && tokens.[0] = Token.Any ":" then 
                    if (List.length tokens) > 1 then parseStatements' list tokens.[1..]
                    else Failure "Expected expression after ':'"
                else Success(Expr.Statements(list), tokens)
            | failure -> failure
        parseStatements' [] tokens
    
    let private parsePrototypeArguments = 
        let rec parsePrototypeArguments' args tokens = 
            if (List.length tokens) <= 0 then Success(args, tokens)
            else 
                match tokens.[0] with
                | Token.Identifier id -> 
                    if (List.length tokens) > 1 then 
                        match tokens.[1] with
                        | Token.Any ":" -> 
                            if (List.length tokens) <= 2 then Failure "Expected type after ':' in argument declaraion"
                            else 
                                match tokens.[2] with
                                | Token.Identifier t -> 
                                    parsePrototypeArguments' (List.append args ((id, Some(t)) :: [])) tokens.[3..]
                                | _ -> Failure "Expected type after ':' in argument declaration"
                        | Token.Identifier _ -> 
                            parsePrototypeArguments' (List.append args ((id, None) :: [])) tokens.[1..]
                        | _ -> Success(List.append args ((id, None) :: []), tokens.[1..])
                    else Success(List.append args ((id, None) :: []), tokens.[1..])
                | _ -> Success(args, tokens)
        parsePrototypeArguments' []
    
    let private parsePrototype = 
        let rec parsePrototype' tokens = 
            if (List.length tokens) <= 0 then Failure "Expected identifier in function declaration"
            else 
                match tokens.[0] with
                | Token.Identifier id -> 
                    if (List.length tokens) <= 1 then Failure "Expected '(' after identifier in function declaration"
                    else 
                        match tokens.[1] with
                        | Token.Any "(" -> 
                            if (List.length tokens) <= 2 then 
                                Failure "Expected arguments or ')' after identifier in function declarartion"
                            else 
                                match parsePrototypeArguments tokens.[2..] with
                                | Success(args, tokens) -> 
                                    if (List.length tokens) <= 0 then 
                                        Failure "Expected ')' after arguments in function declaration"
                                    else 
                                        match tokens.[0] with
                                        | Token.Any ")" -> 
                                            if (List.length tokens) <= 1 then 
                                                Success(Proto.Prototype(id, args, None), tokens.[1..])
                                            else 
                                                match tokens.[1] with
                                                | Token.Any ":" -> 
                                                    if (List.length tokens) <= 2 then 
                                                        Failure "Expected type after ':' in function declaration"
                                                    else 
                                                        match tokens.[2] with
                                                        | Token.Identifier t -> 
                                                            Success(Proto.Prototype(id, args, Some(t)), tokens.[3..])
                                                        | _ -> Success(Proto.Prototype(id, args, None), tokens.[1..])
                                                | _ -> Success(Proto.Prototype(id, args, None), tokens.[1..])
                                        | _ -> Failure "Expected ')' after arguments in function declaration"
                                | Failure msg -> Failure msg
                        | Token.Any ":" -> 
                            if (List.length tokens) <= 2 then Failure "Expected type after ':' in function declaration"
                            else 
                                match tokens.[2] with
                                | Token.Identifier t -> Success(Proto.Prototype(id, [], Some(t)), tokens.[3..])
                                | _ -> Failure "Expected type after ':' in function declaration"
                        | _ -> Success(Proto.Prototype(id, [], None), tokens.[1..])
                | Token.Binary | Token.Unary -> 
                    let (prefix, kind) = 
                        match tokens.[0] with
                        | Token.Binary -> "binary", 2
                        | Token.Unary -> "unary", 1
                    match tokens.[0] with
                    | Token.Any op when op <> "(" && op <> ")" && op <> "," && op <> ":" -> Failure "TODO"
                    | _ -> 
                        Failure
                            (String.concat "" ("Expected operator after " :: prefix :: " in function declaration" :: []))
                | _ -> Failure "Missing identifier in function definition"
        parsePrototype'
    
    let private parseExtern tokens = 
        match parsePrototype tokens with
        | Success(proto, tokens) -> Success(Node.Extern(proto), tokens)
        | Failure msg -> Failure msg
    
    let private parseDef tokens = 
        match parsePrototype tokens with
        | Success(proto, tokens) -> 
            if (List.length tokens) <= 0 then Failure "Expected body expression in function declaration"
            else 
                match parseStatements tokens with
                | Success(expr, tokens) -> Success(Node.Def(proto, expr), tokens)
                | Failure msg -> Failure msg
        | Failure msg -> Failure msg
    
    let parse tokens = 
        let rec parse' (nodes : Node list) tokens = 
            if (List.length tokens) <= 0 then Success nodes
            else 
                match tokens.[0] with
                | Lexer.Def -> 
                    match parseDef tokens.[1..] with
                    | Success(def, tokens) -> 
                        if (List.length tokens) <= 0 || tokens.[0] <> Token.EOS then 
                            Failure "Missing ';' in function definition"
                        else parse' (List.append nodes (def :: [])) tokens.[1..]
                    | Failure msg -> Failure msg
                | Lexer.Extern -> 
                    match parseExtern tokens.[1..] with
                    | Success(ext, tokens) -> 
                        if (List.length tokens) <= 0 || tokens.[0] <> Token.EOS then 
                            Failure "Missing ';' in extern expression"
                        else parse' (List.append nodes (ext :: [])) tokens.[1..]
                    | Failure msg -> Failure msg
                | _ -> 
                    match parseStatements tokens with
                    | Success(expr, tokens) -> 
                        if (List.length tokens) <= 0 || tokens.[0] <> Token.EOS then Failure "Missing ';' in top expr"
                        else parse' (List.append nodes ((Node.TopExpr expr) :: [])) tokens.[1..]
                    | Failure msg -> Failure msg
        parse' [] tokens
