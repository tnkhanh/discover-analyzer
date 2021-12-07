(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

type predicate =
  | NoAlias of (Llvm.llvalue * Llvm.llvalue)
  | MustAlias of (Llvm.llvalue * Llvm.llvalue)
  | MayAlias of (Llvm.llvalue * Llvm.llvalue)
  | RangeLB of (Llvm.llvalue * int64)
  | RangeUB of (Llvm.llvalue * int64)
  | RangeLUB of (Llvm.llvalue * int64 * int64)

type assertion_type =
  | Assert
  | Refute

type assertion =
  { ast_instr : Llir.instr;
    ast_predicate : predicate;
    ast_type : assertion_type
  }

val pr_predicateicate : predicate -> string
val pr_assertion : assertion -> string
val pr_assertion_status : Llir.func -> assertion -> bool -> string

val mk_assertion
  :  assertion_type ->
  predicate ->
  Llir.instr ->
  assertion

val mk_assert : predicate -> Llir.instr -> assertion
val mk_refute : predicate -> Llir.instr -> assertion
val find_alias_assertions : Llir.func -> assertion list
val find_range_assertions : Llir.func -> assertion list
val find_all_assertions : Llir.func -> assertion list
val count_all_assertions : Llir.program -> int
