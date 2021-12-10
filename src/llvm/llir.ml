(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LT = LL.TypeKind
module LV = LL.ValueKind
module LO = LL.Opcode
module SP = Set.Poly

(* Include sub-modules *)

include Llast.AST
include Llast.Value
include Llast.Type
include Llast.Global
include Llast.Expr
include Llast.Instr
include Llast.Opcode
include Llast.Const
include Llast.Func
include Llast.Callable
include Llast.Block
include Llast.Path
include Llast.Loop
include Llast.Program
include Llutils.Iter
include Llutils.Map
include Llutils.Fold
include Llutils.Type
include Llutils.Value
include Llutils.Use
include Llutils.Instr
include Llutils.Global
include Llutils.Block
include Llutils.Func
include Llutils.Path
include Llutils.Metadata
include Llutils.Module
include Llutils.Program
include Llutils.Substitution
