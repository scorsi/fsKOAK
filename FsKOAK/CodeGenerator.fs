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
    let _module = LLVM.ModuleCreateWithName("KOAK")
    let _builder = LLVM.CreateBuilder()
    let _namedValues : Map<string, LLVMValueRef> = Map.empty
    //
    // Types
    //
    let __integer = LLVM.Int64Type()
    let private createInteger v = LLVM.ConstInt(__integer, v |> uint64, LLVMBool(0))
    let __double = LLVM.DoubleType()
    let private createDouble v = LLVM.ConstReal(__double, v)
    let __bool = LLVM.Int1Type()
    let private createBool (v : bool) = LLVM.ConstInt(__bool, System.Convert.ToUInt64(v), LLVMBool(1))
    let __char = LLVM.Int8Type()
    let private createChar v = LLVM.ConstInt(__char, v |> uint64, LLVMBool(0))
    
    //
    // BINARY OPERATOR CODE GENERATOR
    //
    let private genBinarySub lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double then Success(LLVM.BuildFSub(_builder, lhsVal, rhsVal, "sub_tmp"))
        else Success(LLVM.BuildSub(_builder, lhsVal, rhsVal, "sub_tmp"))
    
    let private genBinaryAdd lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double then Success(LLVM.BuildFAdd(_builder, lhsVal, rhsVal, "add_tmp"))
        else Success(LLVM.BuildAdd(_builder, lhsVal, rhsVal, "add_tmp"))
    
    let private genBinaryMul lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double then Success(LLVM.BuildFMul(_builder, lhsVal, rhsVal, "mul_tmp"))
        else Success(LLVM.BuildMul(_builder, lhsVal, rhsVal, "mul_tmp"))
    
    let private genBinaryDiv lhsVal rhsVal = 
        if LLVM.TypeOf(lhsVal) = __double then Success(LLVM.BuildFDiv(_builder, lhsVal, rhsVal, "div_tmp"))
        else Success(LLVM.BuildSDiv(_builder, lhsVal, rhsVal, "div_tmp"))
    
    let private genBinaryCmp lhsVal rhsVal fpred ipred name = 
        if LLVM.TypeOf(lhsVal) = __double then 
            Success
                (LLVM.BuildUIToFP
                     (_builder, (LLVM.BuildFCmp(_builder, fpred, lhsVal, rhsVal, name)), __double, "cast_tmp"))
        else if LLVM.TypeOf(lhsVal) = __integer then 
            Success
                (LLVM.BuildIntCast
                     (_builder, (LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name)), __integer, "cast_tmp"))
        else if LLVM.TypeOf(lhsVal) = __char then 
            Success
                (LLVM.BuildIntCast
                     (_builder, (LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name)), __char, "cast_tmp"))
        else if LLVM.TypeOf(lhsVal) = __bool then 
            Success
                (LLVM.BuildIntCast
                     (_builder, (LLVM.BuildICmp(_builder, ipred, lhsVal, rhsVal, name)), __bool, "cast_tmp"))
        else Failure "Unknown Type"
    
    let private genBinaryLt lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealULT LLVMIntPredicate.LLVMIntSLT "cmp_lt_tmp"
    let private genBinaryGt lhsVal rhsVal = 
        genBinaryCmp lhsVal rhsVal LLVMRealPredicate.LLVMRealUGT LLVMIntPredicate.LLVMIntSGT "cmp_gt_tmp"
    
    //
    // UNARY OPERATOR CODE GENERATOR
    //
    let private genUnarySub lhsVal = 
        if LLVM.TypeOf(lhsVal) = __double then genBinarySub lhsVal (createDouble -1.0)
        else if LLVM.TypeOf(lhsVal) = __integer then genBinarySub lhsVal (createInteger -1)
        else if LLVM.TypeOf(lhsVal) = __char then genBinarySub lhsVal (createChar -1)
        else Failure "Can't apply unary operator on that type"
    
    //
    // EXPR CODE GENERATOR
    //
    let rec private genBinaryExpr op lhs rhs = 
        match genExpr lhs with
        | Success lhsVal -> 
            match genExpr rhs with
            | Success rhsVal -> 
                if LLVM.TypeOf(lhsVal) = LLVM.TypeOf(rhsVal) then 
                    try 
                        let operator = Map.find op Parser.binaryOperators
                        match operator.custom with
                        | true -> genCall ("binary" + op) (lhs :: rhs :: [])
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
    
    and private genUnaryExpr op lhs = 
        match genExpr lhs with
        | Success lhsVal -> 
            match op with
            | "+" -> Success lhsVal
            | "-" -> genUnarySub lhsVal
            | _ -> Failure "TODO"
        | failure -> failure
    
    and private genCall name args = 
        let func = LLVM.GetNamedFunction(_module, name)
        if func.Pointer = IntPtr.Zero then Failure "Unknown function call"
        else if (LLVM.CountParams func) <> ((List.length args) |> uint32) then Failure "Mismatch number of arguments"
        else 
            let args = List.map genExpr args
            
            let rec checkArgs args newArgs = 
                if (List.length args) <= 0 then Success(newArgs)
                else 
                    match args.[0] with
                    | Success v -> checkArgs args.[1..] (List.append newArgs (v :: []))
                    | Failure msg -> Failure msg
            match checkArgs args [] with
            | Success args -> Success(LLVM.BuildCall(_builder, func, (Array.ofList args), "calltmp"))
            | Failure msg -> Failure msg
    
    and private genExpr expr = 
        match expr with
        | Parser.Expr.Integer v -> Success(createInteger v)
        | Parser.Expr.Double v -> Success(createDouble v)
        | Parser.Expr.Boolean v -> Success(createBool v)
        | Parser.Expr.Char v -> Success(createChar (v |> int))
        | Parser.Expr.Variable id -> 
            try 
                Success(Map.find id _namedValues)
            with _ -> Failure("Unknown variable name: " + id)
        | Parser.Expr.Unary(op, lhs) -> genUnaryExpr op lhs
        | Parser.Expr.Binary(op, lhs, rhs) -> genBinaryExpr op lhs rhs
        | Parser.Expr.Call(name, args) -> genCall name args
        | _ -> Failure "TODO"
    
    let codegen (nodes : Parser.Node list) = Failure "Not yet implemented"
