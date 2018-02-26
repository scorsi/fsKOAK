namespace Koak

open LLVMSharp
open System
open System.Collections
open System.Collections.Generic

module CodeGenerator = 
    type Result<'a> = 
        | Success of 'a
        | Failure of string
    
    //
    // LLVM
    //
    let _module = LLVM.ModuleCreateWithName("koak")
    let _builder = LLVM.CreateBuilder()
    
    //
    // Types
    //
    type Type = 
        { typeRef : LLVMTypeRef
          name : string }
    
    let __integer = 
        { typeRef = LLVM.Int64Type()
          name = "int" }
    
    let private createInteger v = LLVM.ConstInt(__integer.typeRef, v |> uint64, LLVMBool(0))
    
    let __double = 
        { typeRef = LLVM.DoubleType()
          name = "double" }
    
    let private createDouble v = LLVM.ConstReal(__double.typeRef, v)
    
    let __bool = 
        { typeRef = LLVM.Int1Type()
          name = "bool" }
    
    let private createBool (v : bool) = LLVM.ConstInt(__bool.typeRef, System.Convert.ToUInt64(v), LLVMBool(1))
    
    let __char = 
        { typeRef = LLVM.Int8Type()
          name = "char" }
    
    let private createChar v = LLVM.ConstInt(__char.typeRef, v |> uint64, LLVMBool(0))
    
    let __void = 
        { typeRef = LLVM.VoidType()
          name = "void" }
    
    let private findType name = 
        match name with
        | "int" -> Success(__integer.typeRef)
        | "double" -> Success(__double.typeRef)
        | "bool" -> Success(__bool.typeRef)
        | "char" -> Success(__char.typeRef)
        | "void" -> Success(__void.typeRef)
        | _ -> Failure "Unknown type"
    
    //
    // UTILS
    //
    let private genZero ofVal = 
        if LLVM.TypeOf(ofVal) = __double.typeRef then createDouble 0.0
        else if LLVM.TypeOf(ofVal) = __integer.typeRef then createInteger 0
        else if LLVM.TypeOf(ofVal) = __char.typeRef then createChar 0
        else createBool false
    
    let private genOne ofVal = 
        if LLVM.TypeOf(ofVal) = __double.typeRef then createDouble 1.0
        else if LLVM.TypeOf(ofVal) = __integer.typeRef then createInteger 1
        else if LLVM.TypeOf(ofVal) = __char.typeRef then createChar 1
        else createBool true
    
    //
    // BINARY OPERATOR CODE GENERATOR
    //
    let rec private genBinarySub lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFSub(_builder, lhsVal, rhsVal, "sub_tmp"))
        else Success(LLVM.BuildSub(_builder, lhsVal, rhsVal, "sub_tmp"))
    
    and private genBinaryAdd lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFAdd(_builder, lhsVal, rhsVal, "add_tmp"))
        else Success(LLVM.BuildAdd(_builder, lhsVal, rhsVal, "add_tmp"))
    
    and private genBinaryMul lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFMul(_builder, lhsVal, rhsVal, "mul_tmp"))
        else Success(LLVM.BuildMul(_builder, lhsVal, rhsVal, "mul_tmp"))
    
    and private genBinaryDiv lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFDiv(_builder, lhsVal, rhsVal, "div_tmp"))
        else Success(LLVM.BuildSDiv(_builder, lhsVal, rhsVal, "div_tmp"))
    
    and private genBinaryCmp lhsVal rhsVal fpred ipred name = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFCmp(_builder, fpred, lhsVal, rhsVal, name))
        else if LLVM.TypeOf(lhsVal) = __integer.typeRef then 
            Success(LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name))
        else if LLVM.TypeOf(lhsVal) = __char.typeRef then Success(LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name))
        else if LLVM.TypeOf(lhsVal) = __bool.typeRef then Success(LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name))
        else Failure "Unknown Type"
    
    and private genBinaryCmpLt lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealULT LLVMIntPredicate.LLVMIntSLT "cmp_lt_tmp"
    
    and private genBinaryCmpGt lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealUGT LLVMIntPredicate.LLVMIntSGT "cmp_gt_tmp"
    
    and private genBinaryCmpEq lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealUEQ LLVMIntPredicate.LLVMIntEQ "cmp_eq_tmp"
    
    and private genBinaryCmpNe lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealUNE LLVMIntPredicate.LLVMIntNE "cmp_ne_tmp"
    
    and private genBinaryCmpAnd lhsVal rhsVal = 
        match genBinaryCmpNe lhsVal (genZero lhsVal) with
        | Success lhsValRes -> 
            match genUnaryCast lhsValRes __integer.typeRef with
            | Success lhsValRes -> 
                match genBinaryCmpNe rhsVal (genZero rhsVal) with
                | Success rhsValRes -> 
                    match genUnaryCast rhsValRes __integer.typeRef with
                    | Success rhsValRes -> 
                        match genBinaryAdd lhsValRes rhsValRes with
                        | Success addRes -> genBinaryCmpEq addRes (createInteger 2)
                        | failure -> failure
                    | failure -> failure
                | failure -> failure
            | failure -> failure
        | failure -> failure
    
    and private genBinaryCmpOr lhsVal rhsVal = 
        match genBinaryCmpNe lhsVal (genZero lhsVal) with
        | Success lhsValRes -> 
            match genUnaryCast lhsValRes __integer.typeRef with
            | Success lhsValRes -> 
                match genBinaryCmpNe rhsVal (genZero rhsVal) with
                | Success rhsValRes -> 
                    match genUnaryCast rhsValRes __integer.typeRef with
                    | Success rhsValRes -> 
                        match genBinaryAdd lhsValRes rhsValRes with
                        | Success addRes -> genBinaryCmpGt addRes (createInteger 0)
                        | failure -> failure
                    | failure -> failure
                | failure -> failure
            | failure -> failure
        | failure -> failure
    
    and private genBinaryOr lhsVal rhsVal = Success(LLVM.BuildOr(_builder, lhsVal, rhsVal, "or_tmp"))
    
    and private genBinaryAnd lhsVal rhsVal = Success(LLVM.BuildAnd(_builder, lhsVal, rhsVal, "and_tmp"))
    
    and private genBinaryXor lhsVal rhsVal = Success(LLVM.BuildXor(_builder, lhsVal, rhsVal, "xor_tmp"))
    
    //
    // UNARY OPERATOR CODE GENERATOR
    //
    and private genUnarySub lhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then genBinarySub lhsVal (createDouble -1.0)
        else if LLVM.TypeOf(lhsVal) = __integer.typeRef then genBinarySub lhsVal (createInteger -1)
        else if LLVM.TypeOf(lhsVal) = __char.typeRef then genBinarySub lhsVal (createChar -1)
        else Failure "Can't apply unary operator on that type"
    
    and private genUnaryCast lhsVal toType = 
        if toType <> __double.typeRef && toType <> __void.typeRef then 
            Success(LLVM.BuildIntCast(_builder, lhsVal, toType, "cast_tmp"))
        else if toType = __double.typeRef && LLVM.TypeOf(lhsVal) <> __double.typeRef then 
            Success(LLVM.BuildUIToFP(_builder, lhsVal, toType, "cast_tmp"))
        else if toType = __double.typeRef then Success(LLVM.BuildFPCast(_builder, lhsVal, toType, "cast_tmp"))
        else Failure "Can't cast to void type"
    
    //
    // EXPR CODE GENERATOR
    //
    let rec private genBinaryExpr namedValues op lhs rhs = 
        match genExpr namedValues lhs with
        | Success lhsVal -> 
            match genExpr namedValues rhs with
            | Success rhsVal -> 
                if LLVM.TypeOf(lhsVal) = LLVM.TypeOf(rhsVal) then 
                    try 
                        let operator = Map.find op Parser.binaryOperators
                        match operator.custom with
                        | true -> genCall namedValues ("binary" + op) (lhs :: rhs :: [])
                        | false -> 
                            match op with
                            | "+" -> genBinaryAdd lhsVal rhsVal
                            | "-" -> genBinarySub lhsVal rhsVal
                            | "*" -> genBinaryMul lhsVal rhsVal
                            | "/" -> genBinaryDiv lhsVal rhsVal
                            | "<" -> 
                                match genBinaryCmpLt lhsVal rhsVal with
                                | Success(v) -> genUnaryCast v (LLVM.TypeOf(lhsVal))
                                | Failure msg -> Failure msg
                            | ">" -> 
                                match genBinaryCmpGt lhsVal rhsVal with
                                | Success(v) -> genUnaryCast v (LLVM.TypeOf(lhsVal))
                                | Failure msg -> Failure msg
                            | "==" -> 
                                match genBinaryCmpEq lhsVal rhsVal with
                                | Success(v) -> genUnaryCast v (LLVM.TypeOf(lhsVal))
                                | Failure msg -> Failure msg
                            | "!=" -> 
                                match genBinaryCmpNe lhsVal rhsVal with
                                | Success(v) -> genUnaryCast v (LLVM.TypeOf(lhsVal))
                                | Failure msg -> Failure msg
                            | "|" -> genBinaryOr lhsVal rhsVal
                            | "&" -> genBinaryAnd lhsVal rhsVal
                            | "^" -> genBinaryXor lhsVal rhsVal
                            | "&&" -> 
                                match genBinaryCmpAnd lhsVal rhsVal with
                                | Success(v) -> genUnaryCast v (LLVM.TypeOf(lhsVal))
                                | Failure msg -> Failure msg
                            | "||" -> 
                                match genBinaryCmpOr lhsVal rhsVal with
                                | Success(v) -> genUnaryCast v (LLVM.TypeOf(lhsVal))
                                | Failure msg -> Failure msg
                            | _ -> Failure "genBinaryExpr TODO"
                    with _ -> Failure "Unkown binary operator"
                else Failure "Types mismatch in binary operation"
            | failure -> failure
        | failure -> failure
    
    and private genUnaryExpr namedValues op lhs = 
        match genExpr namedValues lhs with
        | Success lhsVal -> 
            match op with
            | "+" -> Success lhsVal
            | "-" -> genUnarySub lhsVal
            | _ -> Failure "genUnaryExpr TODO"
        | failure -> failure
    
    and private genCall namedValues name args = 
        let func = LLVM.GetNamedFunction(_module, name)
        if func.Pointer = IntPtr.Zero then Failure "Unknown function call"
        else if (LLVM.CountParams func) <> ((List.length args) |> uint32) then Failure "Mismatch number of arguments"
        else 
            let args = List.map (genExpr namedValues) args
            
            let rec checkArgs args newArgs = 
                if (List.length args) <= 0 then Success(newArgs)
                else 
                    match args.[0] with
                    | Success v -> checkArgs args.[1..] (List.append newArgs (v :: []))
                    | Failure msg -> Failure msg
            match checkArgs args [] with
            | Success args -> 
                if LLVM.GetReturnType(LLVM.GetElementType(LLVM.TypeOf(func))) = __void.typeRef then 
                    Success(LLVM.BuildCall(_builder, func, (Array.ofList args), ""))
                else Success(LLVM.BuildCall(_builder, func, (Array.ofList args), "call_tmp"))
            | Failure msg -> Failure msg
    
    and private genIfExpr namedValues condExpr thenExpr elseExpr = 
        match genExpr namedValues condExpr with
        | Success(condExprVal) -> 
            match genBinaryCmpNe condExprVal (genZero condExprVal) with
            | Success(condExprVal) -> 
                let startBlock = LLVM.GetInsertBlock(_builder)
                let func = LLVM.GetBasicBlockParent(startBlock)
                let thenBlock = LLVM.AppendBasicBlock(func, "then")
                LLVM.PositionBuilderAtEnd(_builder, thenBlock)
                match genExpr namedValues thenExpr with
                | Success(thenExprVal) -> 
                    if LLVM.TypeOf(thenExprVal) = __void.typeRef then 
                        Failure "'then' expression in if expression must return a value"
                    else 
                        let endThenBlock = LLVM.GetInsertBlock(_builder)
                        let elseBlock = LLVM.AppendBasicBlock(func, "else")
                        LLVM.PositionBuilderAtEnd(_builder, elseBlock)
                        match genExpr namedValues elseExpr with
                        | Success(elseExprVal) -> 
                            if LLVM.TypeOf(elseExprVal) = __void.typeRef then 
                                Failure "'else' expression in if expression must return a value"
                            else if LLVM.TypeOf(thenExprVal) <> LLVM.TypeOf(elseExprVal) then 
                                Failure 
                                    "'then' expression and 'else' expression in if expression must be of the same type"
                            else 
                                let endElseBlock = LLVM.GetInsertBlock(_builder)
                                let mergeBlock = LLVM.AppendBasicBlock(func, "ifcont")
                                LLVM.PositionBuilderAtEnd(_builder, mergeBlock)
                                let phi = LLVM.BuildPhi(_builder, LLVM.TypeOf(thenExprVal), "if_tmp")
                                LLVM.AddIncoming
                                    (phi, Array.ofList (thenExprVal :: []), Array.ofList (endThenBlock :: []), 
                                     1 |> uint32)
                                LLVM.AddIncoming
                                    (phi, Array.ofList (elseExprVal :: []), Array.ofList (endElseBlock :: []), 
                                     1 |> uint32)
                                LLVM.PositionBuilderAtEnd(_builder, startBlock)
                                LLVM.BuildCondBr(_builder, condExprVal, thenBlock, elseBlock) |> ignore
                                LLVM.PositionBuilderAtEnd(_builder, endThenBlock)
                                LLVM.BuildBr(_builder, mergeBlock) |> ignore
                                LLVM.PositionBuilderAtEnd(_builder, endElseBlock)
                                LLVM.BuildBr(_builder, mergeBlock) |> ignore
                                LLVM.PositionBuilderAtEnd(_builder, mergeBlock)
                                Success(phi)
                        | failure -> failure
                | failure -> failure
            | failure -> failure
        | failure -> failure
    
    and private genForExpr namedValues id assignExpr condExpr stepExpr bodyExpr = 
        match genExpr namedValues assignExpr with
        | Success(assignExprVal) -> 
            let preheaderBlock = LLVM.GetInsertBlock(_builder)
            let func = LLVM.GetBasicBlockParent(preheaderBlock)
            let loopBlock = LLVM.AppendBasicBlock(func, "loop")
            LLVM.BuildBr(_builder, loopBlock) |> ignore
            LLVM.PositionBuilderAtEnd(_builder, loopBlock)
            let variable = LLVM.BuildPhi(_builder, LLVM.TypeOf(assignExprVal), id)
            LLVM.AddIncoming
                (variable, Array.ofList (assignExprVal :: []), Array.ofList (preheaderBlock :: []), 1 |> uint32)
            match Map.containsKey id namedValues with
            | true -> Failure "Redeclaration of variable in for expression"
            | false -> 
                let namedValues = namedValues.Add(id, variable)
                match genExpr namedValues bodyExpr with
                | Success(_) -> 
                    match (match stepExpr with
                           | Some(stepExpr) -> genExpr namedValues stepExpr
                           | None -> Success(genOne variable)) with
                    | Success(stepExprVal) -> 
                        match genBinaryAdd variable stepExprVal with
                        | Success(nextVal) -> 
                            match genExpr namedValues condExpr with
                            | Success(condExprVal) -> 
                                match genBinaryCmpNe condExprVal (genZero condExprVal) with
                                | Success(condExprVal) -> 
                                    let loopEndBlock = LLVM.GetInsertBlock(_builder)
                                    let afterBlock = LLVM.AppendBasicBlock(func, "after_loop")
                                    LLVM.BuildCondBr(_builder, condExprVal, loopBlock, afterBlock) |> ignore
                                    LLVM.PositionBuilderAtEnd(_builder, afterBlock)
                                    LLVM.AddIncoming
                                        (variable, (Array.ofList (nextVal :: [])), (Array.ofList (loopEndBlock :: [])), 
                                         1 |> uint32)
                                    Success(variable)
                                | Failure msg -> Failure msg
                            | Failure msg -> Failure msg
                        | Failure msg -> Failure msg
                    | Failure msg -> Failure msg
                | Failure msg -> Failure msg
        | Failure msg -> Failure msg
    
    and private genExpr namedValues expr = 
        match expr with
        | Parser.Expr.Integer v -> Success(createInteger v)
        | Parser.Expr.Double v -> Success(createDouble v)
        | Parser.Expr.Boolean v -> Success(createBool v)
        | Parser.Expr.Char v -> Success(createChar (v |> int))
        | Parser.Expr.Variable id -> 
            try 
                Success(Map.find id namedValues)
            with _ -> Failure("Unknown variable name: " + id)
        | Parser.Expr.Unary(op, lhs) -> genUnaryExpr namedValues op lhs
        | Parser.Expr.Binary(op, lhs, rhs) -> genBinaryExpr namedValues op lhs rhs
        | Parser.Expr.Call(name, args) -> genCall namedValues name args
        | Parser.Expr.Statements(s) -> genStatements namedValues s
        | Parser.Expr.If(condExpr, thenExpr, elseExpr) -> genIfExpr namedValues condExpr thenExpr elseExpr
        | Parser.Expr.For(id, assignExpr, condExpr, stepExpr, bodyExpr) -> 
            genForExpr namedValues id assignExpr condExpr stepExpr bodyExpr
        | _ -> Failure "genExpr TODO"
    
    and private genStatements namedValues expr = 
        if (List.length expr) <= 0 then Failure "Empty statement list"
        else 
            let rec genStatements' (expr : Parser.Expr list) retVal = 
                if (List.length expr) <= 0 then Success(retVal)
                else 
                    match genExpr namedValues expr.[0] with
                    | Success(retVal) -> genStatements' expr.[1..] retVal
                    | Failure msg -> Failure msg
            genStatements' expr (createInteger 0)
    
    let genProto name args retType = 
        let func = LLVM.GetNamedFunction(_module, name)
        if func.Pointer <> IntPtr.Zero then 
            if LLVM.CountParams(func) <> ((List.length args) |> uint32) then 
                Failure "Function already declared with different number of args"
            else if LLVM.CountBasicBlocks(func) <> (0 |> uint32) then Failure "Function already declared"
            else Failure "TODO"
        else 
            let rec genDefArguments args argsType = 
                if (List.length args) <= 0 then Success(argsType)
                else 
                    match args.[0] with
                    | (_, Some(t)) -> 
                        match findType t with
                        | Success(t) -> genDefArguments args.[1..] (List.append argsType (t :: []))
                        | Failure msg -> Failure msg
                    | (_, None) -> Failure "Type inference not yet handled"
            match genDefArguments args [] with
            | Success(argsType) -> 
                match retType with
                | Some(retType) -> 
                    match findType retType with
                    | Success(retType) -> 
                        let func = 
                            LLVM.AddFunction(_module, name, LLVM.FunctionType(retType, (Array.ofList argsType), false))
                        LLVM.SetLinkage(func, LLVMLinkage.LLVMExternalLinkage)
                        let rec setFunctionArgument i args (namedValues : Map<string, LLVMValueRef>) = 
                            if (List.length args) <= i then namedValues
                            else 
                                let (argName, _) = args.[i]
                                let param = LLVM.GetParam(func, (i |> uint32))
                                LLVM.SetValueName(param, argName)
                                setFunctionArgument (i + 1) args (namedValues.Add(argName, param))
                        Success(func, setFunctionArgument 0 args Map.empty)
                    | Failure msg -> Failure msg
                | None -> Failure "Type inference not yet handled"
            | Failure msg -> Failure msg
    
    let genDef name args retType body = 
        match genProto name args retType with
        | Success(func, namedValues) -> 
            LLVM.PositionBuilderAtEnd(_builder, LLVM.AppendBasicBlock(func, "entry"))
            match genExpr namedValues body with
            | Success(retVal) -> 
                let retType = 
                    match retType with
                    | Some(retType) -> 
                        match findType retType with
                        | Success(retType) -> retType
                        | Failure msg -> __void.typeRef
                    | _ -> __void.typeRef
                if retType = __void.typeRef then LLVM.BuildRetVoid(_builder)
                else LLVM.BuildRet(_builder, retVal)
                |> ignore
                if LLVM.VerifyFunction(func, LLVMVerifierFailureAction.LLVMPrintMessageAction) = LLVMBool(0) then 
                    Success()
                else 
                    //                    LLVM.DeleteFunction(func)
                    Failure "Function verification failed"
            | Failure msg -> 
                //                LLVM.DeleteFunction(func)
                Failure msg
        | Failure msg -> Failure msg
    
    let codegen (nodes : Parser.Node list) = 
        LLVM.LinkInMCJIT()
        LLVM.InitializeX86TargetInfo()
        LLVM.InitializeX86Target()
        LLVM.InitializeX86TargetMC()
        let mutable engine = LLVMExecutionEngineRef()
        let mutable error = ""
        LLVM.CreateExecutionEngineForModule(&engine, _module, &error) |> ignore
        let passManager = LLVM.CreateFunctionPassManagerForModule(_module)
        LLVM.AddBasicAliasAnalysisPass(passManager)
        LLVM.AddPromoteMemoryToRegisterPass(passManager)
        LLVM.AddInstructionCombiningPass(passManager)
        LLVM.AddReassociatePass(passManager)
        LLVM.AddGVNPass(passManager)
        LLVM.AddCFGSimplificationPass(passManager)
        LLVM.InitializeFunctionPassManager(passManager) |> ignore
        let rec codegen' nodes = 
            if (List.length nodes) <= 0 then Success()
            else 
                match nodes.[0] with
                | Parser.Node.Def(proto, expr) -> 
                    match proto with
                    | Parser.Proto.Prototype(name, args, retType) -> 
                        match genDef name args retType expr with
                        | Success _ -> codegen' nodes.[1..]
                        | Failure msg -> Failure msg
                    | _ -> Failure "TODO"
                | Parser.Node.Extern(proto) -> 
                    match proto with
                    | Parser.Proto.Prototype(name, args, retType) -> 
                        match genProto name args retType with
                        | Success _ -> codegen' nodes.[1..]
                        | Failure msg -> Failure msg
                    | _ -> Failure "Unknown error"
                | _ -> Failure "TODO"
        match codegen' nodes with
        | Success _ ->
            LLVM.WriteBitcodeToFile(_module, "out.bitcode") |> ignore
            LLVM.PrintModuleToFile(_module, "out.ir") |> ignore
            Success()
        | failure -> failure
