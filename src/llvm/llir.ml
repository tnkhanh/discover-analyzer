(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LL = Llvm
module LT = Llvm.TypeKind
module LV = Llvm.ValueKind
module LO = Llvm.Opcode
module SP = Set.Poly

(* Include core sub-modules *)

include Llast.AST
include Llast.Value
include Llast.Type
include Llast.Global
include Llast.Expr
include Llast.Pointer
include Llast.Instr
include Llast.Use
include Llast.Opcode
include Llast.Const
include Llast.Param
include Llast.Func
include Llast.Callable
include Llast.Block
include Llast.Path
include Llast.Loop
include Llast.Metadata
include Llast.Module
include Llast.Program

(* Include utility sub-modules *)

include Llutils.Iter
include Llutils.Map
include Llutils.Fold
include Llutils.Exists
include Llutils.ForAll
include Llutils.Visit
include Llutils.VisitFold
include Llutils.VisitExists
include Llutils.VisitForAll
include Llutils.Type
include Llutils.Value
include Llutils.Use
include Llutils.Instr
include Llutils.Global
include Llutils.Block
(* include Llutils.Param *)
include Llutils.Func
include Llutils.Path
include Llutils.Metadata
include Llutils.Module
include Llutils.Program
include Llutils.Substitution
