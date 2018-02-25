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
    let _module = LLVM.ModuleCreateWithName("KOAK_LLVM_module_1")
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
    
    let private findType name = 
        match name with
        | "int" -> Success(__integer.typeRef)
        | "double" -> Success(__double.typeRef)
        | "bool" -> Success(__bool.typeRef)
        | "char" -> Success(__char.typeRef)
        | _ -> Failure "Unknown type"
    
    //
    // BINARY OPERATOR CODE GENERATOR
    //
    let private genBinarySub lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFSub(_builder, lhsVal, rhsVal, "sub_tmp"))
        else Success(LLVM.BuildSub(_builder, lhsVal, rhsVal, "sub_tmp"))
    
    let private genBinaryAdd lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFAdd(_builder, lhsVal, rhsVal, "add_tmp"))
        else Success(LLVM.BuildAdd(_builder, lhsVal, rhsVal, "add_tmp"))
    
    let private genBinaryMul lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFMul(_builder, lhsVal, rhsVal, "mul_tmp"))
        else Success(LLVM.BuildMul(_builder, lhsVal, rhsVal, "mul_tmp"))
    
    let private genBinaryDiv lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then Success(LLVM.BuildFDiv(_builder, lhsVal, rhsVal, "div_tmp"))
        else Success(LLVM.BuildSDiv(_builder, lhsVal, rhsVal, "div_tmp"))
    
    let private genBinaryCmp lhsVal rhsVal fpred ipred name = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then 
            Success
                (LLVM.BuildUIToFP
                     (_builder, (LLVM.BuildFCmp(_builder, fpred, lhsVal, rhsVal, name)), __double.typeRef, "cast_tmp"))
        else if LLVM.TypeOf(lhsVal) = __integer.typeRef then 
            Success
                (LLVM.BuildIntCast
                     (_builder, (LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name)), __integer.typeRef, "cast_tmp"))
        else if LLVM.TypeOf(lhsVal) = __char.typeRef then 
            Success
                (LLVM.BuildIntCast
                     (_builder, (LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name)), __char.typeRef, "cast_tmp"))
        else if LLVM.TypeOf(lhsVal) = __bool.typeRef then 
            Success
                (LLVM.BuildIntCast
                     (_builder, (LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name)), __bool.typeRef, "cast_tmp"))
        else Failure "Unknown Type"
    
    let private genBinaryLt lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealULT LLVMIntPredicate.LLVMIntSLT "cmp_lt_tmp"
    let private genBinaryGt lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealUGT LLVMIntPredicate.LLVMIntSGT "cmp_gt_tmp"
    
    //
    // UNARY OPERATOR CODE GENERATOR
    //
    let private genUnarySub lhsVal = 
        if LLVM.TypeOf(lhsVal) = __double.typeRef then genBinarySub lhsVal (createDouble -1.0)
        else if LLVM.TypeOf(lhsVal) = __integer.typeRef then genBinarySub lhsVal (createInteger -1)
        else if LLVM.TypeOf(lhsVal) = __char.typeRef then genBinarySub lhsVal (createChar -1)
        else Failure "Can't apply unary operator on that type"
    
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
                            | "<" -> genBinaryLt lhsVal rhsVal
                            | ">" -> genBinaryGt lhsVal rhsVal
                            | _ -> Failure "TODO"
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
            | _ -> Failure "TODO"
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
            | Success args -> Success(LLVM.BuildCall(_builder, func, (Array.ofList args), "calltmp"))
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
        | _ -> Failure "TODO"
    
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
    
    let genDef name args retType body = 
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
                        LLVM.PositionBuilderAtEnd(_builder, LLVM.AppendBasicBlock(func, "entry"))
                        match genExpr (setFunctionArgument 0 args Map.empty) body with
                        | Success(retVal) -> 
                            LLVM.BuildRet(_builder, retVal) |> ignore
                            if LLVM.VerifyFunction(func, LLVMVerifierFailureAction.LLVMPrintMessageAction) = LLVMBool(0) then 
                                Success()
                            else 
                                LLVM.DeleteFunction(func)
                                Failure "Function verification failed"
                        | Failure msg -> 
                            LLVM.DeleteFunction(func)
                            Failure msg
                    | Failure msg -> Failure msg
                | None -> Failure "Type inference not yet handled"
            | Failure msg -> Failure msg
    
    let codegen (nodes : Parser.Node list) = 
        let rec codegen' nodes = 
            if (List.length nodes) <= 0 then Success()
            else 
                match nodes.[0] with
                | Parser.Node.Def(proto, expr) -> 
                    match proto with
                    | Parser.Proto.Prototype(name, args, retType) -> 
                        match genDef name args retType expr with
                        | Success _ -> 
                            LLVM.DumpModule(_module)
                            Success()
                        | Failure msg -> 
                            LLVM.DumpModule(_module)
                            Failure msg
                    | _ -> Failure "TODO"
                | _ -> Failure "TODO"
        codegen' nodes
