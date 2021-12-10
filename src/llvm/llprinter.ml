(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module LL = Llvm
module LO = LL.Opcode
module LV = LL.ValueKind
module LT = LL.TypeKind

(*******************************************************************
 ** primitive printers of LLVM data structures
 *******************************************************************)

module PrinterPrimitives = struct
  let pr_opcode (op : LO.t) =
    match op with
    | LO.Invalid -> "Invalid"
    | LO.Ret -> "Ret"
    | LO.Br -> "Br"
    | LO.Switch -> "Switch"
    | LO.IndirectBr -> "IndirectBr"
    | LO.Invoke -> "Invoke"
    | LO.Invalid2 -> "Invalid2"
    | LO.Unreachable -> "Unreachable"
    | LO.Add -> "Add"
    | LO.FAdd -> "FAdd"
    | LO.Sub -> "Sub"
    | LO.FSub -> "FSub"
    | LO.Mul -> "Mul"
    | LO.FMul -> "FMul"
    | LO.UDiv -> "UDiv"
    | LO.SDiv -> "SDiv"
    | LO.FDiv -> "FDiv"
    | LO.URem -> "URem"
    | LO.SRem -> "SRem"
    | LO.FRem -> "FRem"
    | LO.Shl -> "Shl"
    | LO.LShr -> "LShr"
    | LO.AShr -> "AShr"
    | LO.And -> "And"
    | LO.Or -> "Or"
    | LO.Xor -> "Xor"
    | LO.Alloca -> "Alloca"
    | LO.Load -> "Load"
    | LO.Store -> "Store"
    | LO.GetElementPtr -> "GetElementPtr"
    | LO.Trunc -> "Trunc"
    | LO.ZExt -> "ZExt"
    | LO.SExt -> "SExt"
    | LO.FPToUI -> "FPToUI"
    | LO.FPToSI -> "FPToSI"
    | LO.UIToFP -> "UIToFP"
    | LO.SIToFP -> "SIToFP"
    | LO.FPTrunc -> "FPTrunc"
    | LO.FPExt -> "FPExt"
    | LO.PtrToInt -> "PtrToInt"
    | LO.IntToPtr -> "IntToPtr"
    | LO.BitCast -> "BitCast"
    | LO.ICmp -> "ICmp"
    | LO.FCmp -> "FCmp"
    | LO.PHI -> "PHI"
    | LO.Call -> "Call"
    | LO.Select -> "Select"
    | LO.UserOp1 -> "UserOp1"
    | LO.UserOp2 -> "UserOp2"
    | LO.VAArg -> "VAArg"
    | LO.ExtractElement -> "ExtractElement"
    | LO.InsertElement -> "InsertElement"
    | LO.ShuffleVector -> "ShuffleVector"
    | LO.ExtractValue -> "ExtractValue"
    | LO.InsertValue -> "InsertValue"
    | LO.Fence -> "Fence"
    | LO.AtomicCmpXchg -> "AtomicCmpXchg"
    | LO.AtomicRMW -> "AtomicRMW"
    | LO.Resume -> "Resume"
    | LO.LandingPad -> "LandingPad"
    | _ -> "UnknownOpcode"
  ;;

  let pr_valuekind (k : LV.t) : string =
    match k with
    | LV.NullValue -> "NullValue"
    | LV.Argument -> "Argument"
    | LV.BasicBlock -> "BasicBlock"
    | LV.InlineAsm -> "InlineAsm"
    | LV.MDNode -> "MDNode"
    | LV.MDString -> "MDString"
    | LV.BlockAddress -> "BlockAddress"
    | LV.ConstantAggregateZero -> "ConstantAggregateZero"
    | LV.ConstantArray -> "ConstantArray"
    | LV.ConstantDataArray -> "ConstantDataArray"
    | LV.ConstantDataVector -> "ConstantDataVector"
    | LV.ConstantExpr -> "ConstantExpr"
    | LV.ConstantFP -> "ConstantFP"
    | LV.ConstantInt -> "ConstantInt"
    | LV.ConstantPointerNull -> "ConstantPointerNull"
    | LV.ConstantStruct -> "ConstantStruct"
    | LV.ConstantVector -> "ConstantVector"
    | LV.Function -> "Function"
    | LV.GlobalAlias -> "GlobalAlias"
    | LV.GlobalIFunc -> "GlobalIFunc"
    | LV.GlobalVariable -> "GlobalVariable"
    | LV.UndefValue -> "UndefValue"
    | LV.PoisonValue -> "PoisonValue"
    | LV.Instruction _ -> "Instruction"
  ;;

  let pr_typekind (k : LT.t) : string =
    match k with
    | LT.Void -> "Void"
    | LT.Half -> "Half"
    | LT.Float -> "Float"
    | LT.Double -> "Double"
    | LT.X86fp80 -> "X86fp80"
    | LT.Fp128 -> "Fp128"
    | LT.Ppc_fp128 -> "Ppc_fp128"
    | LT.Label -> "Label"
    | LT.Integer -> "Integer"
    | LT.Function -> "Function"
    | LT.Struct -> "Struct"
    | LT.Array -> "Array"
    | LT.Pointer -> "Pointer"
    | LT.Vector -> "Vector"
    | LT.Metadata -> "Metadata"
    | LT.X86_mmx -> "X86_mmx"
    | LT.Token -> "Token"
    | LT.ScalableVector -> "ScalableVector"
    | LT.BFloat -> "BFloat"
    | LT.X86_amx -> "X86_amx"
  ;;

  let pr_icmp (cmp : LL.Icmp.t) : string =
    match cmp with
    | LL.Icmp.Eq -> "="
    | LL.Icmp.Ne -> "!="
    | LL.Icmp.Ugt -> ">"
    | LL.Icmp.Uge -> ">="
    | LL.Icmp.Ult -> "<"
    | LL.Icmp.Ule -> "<="
    | LL.Icmp.Sgt -> ">"
    | LL.Icmp.Sge -> ">="
    | LL.Icmp.Slt -> "<"
    | LL.Icmp.Sle -> "<="
  ;;

  let pr_fcmp (cmp : LL.Fcmp.t) : string =
    match cmp with
    | LL.Fcmp.False -> "False"
    | LL.Fcmp.Oeq -> "=="
    | LL.Fcmp.Ogt -> ">"
    | LL.Fcmp.Oge -> ">="
    | LL.Fcmp.Olt -> "<"
    | LL.Fcmp.Ole -> "<="
    | LL.Fcmp.One -> "!="
    | LL.Fcmp.Ord -> "NaN"
    | LL.Fcmp.Uno -> "NaN"
    | LL.Fcmp.Ueq -> "=="
    | LL.Fcmp.Ugt -> ">"
    | LL.Fcmp.Uge -> ">="
    | LL.Fcmp.Ult -> "<"
    | LL.Fcmp.Ule -> "<="
    | LL.Fcmp.Une -> "!="
    | LL.Fcmp.True -> "True"
  ;;
end
