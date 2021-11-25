(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Printer
module LL = Llvm
module LI = Llir
module LD = Lldebug
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LA = List.Assoc

(*******************************************************************
 ** data structure
 *******************************************************************)

type predicate =
  (* alias *)
  | NoAlias of (LL.llvalue * LL.llvalue)
  | MustAlias of (LL.llvalue * LL.llvalue)
  | MayAlias of (LL.llvalue * LL.llvalue)
  (* range *)
  (* | RangeLB of (LL.llvalue * BInt.big_int) *)
  | RangeLB of (LL.llvalue * int64)
  | RangeUB of (LL.llvalue * int64)
  | RangeLUB of (LL.llvalue * int64 * int64)

type assertion_type =
  | Assert
  | Refute

type assertion =
  { ast_instr : LI.instr;
    ast_predicate : predicate;
    ast_type : assertion_type
  }

(*******************************************************************
 ** printing
 *******************************************************************)

let pr_predicateicate (pred : predicate) : string =
  match pred with
  | NoAlias (u, v) -> "NoAlias(" ^ LI.pr_value u ^ ", " ^ LI.pr_value v ^ ")"
  | MayAlias (u, v) -> "MayAlias(" ^ LI.pr_value u ^ ", " ^ LI.pr_value v ^ ")"
  | MustAlias (u, v) ->
    "MustAlias(" ^ LI.pr_value u ^ ", " ^ LI.pr_value v ^ ")"
  | _ -> "pr_predicateicate: to implement"
;;

let pr_assertion (ast : assertion) : string = LI.pr_instr ast.ast_instr

let pr_assertion_status (func : LI.func) (ast : assertion) (status : bool) =
  let instr = ast.ast_instr in
  let func_name = LI.func_name func in
  let assertion =
    match LI.is_instr_call_invoke instr with
    | false -> herror "assertion must be a function call: " LI.pr_instr instr
    | true ->
      let asname = LI.func_name (LI.callee_of_instr_func_call instr) in
      let args = LI.args_of_instr_func_app instr in
      asname ^ "(" ^ pr_args ~f:LI.pr_value args ^ ")" in
  let location =
    match LD.position_of_instr instr with
    | None -> "Function: " ^ func_name
    | Some l ->
      let file_name, line = l.pos_file_name, l.pos_line_start in
      let location =
        [ "File: " ^ file_name;
          "function: " ^ func_name;
          "line: " ^ pr_int line
        ] in
      String.concat ~sep:", " location in
  location ^ "\n  " ^ assertion ^ if status then ": OK!" else ": FAILED!"
;;

(*******************************************************************
 ** constructors
 *******************************************************************)

let mk_assertion assertion_type (pred : predicate) (instr : LI.instr) =
  { ast_instr = instr; ast_predicate = pred; ast_type = assertion_type }
;;

let mk_assert (pred : predicate) (instr : LI.instr) =
  { ast_instr = instr; ast_predicate = pred; ast_type = Assert }
;;

let mk_refute (pred : predicate) (instr : LI.instr) =
  { ast_instr = instr; ast_predicate = pred; ast_type = Refute }
;;

let find_alias_assertions (func : LI.func) : assertion list =
  let finstr =
    Some
      (fun acc instr ->
        let vinstr = LI.llvalue_of_instr instr in
        match LL.instr_opcode vinstr with
        | LO.Call | LO.Invoke ->
          let callee = LI.callee_of_instr_func_call instr in
          let fname = LI.func_name callee in
          if String.is_substring fname ~substring:__assert_no_alias
          then
            let u, v = LI.operand instr 0, LI.operand instr 1 in
            acc @ [ mk_assert (NoAlias (u, v)) instr ]
          else if String.is_substring fname ~substring:__refute_no_alias
          then
            let u, v = LI.operand instr 0, LI.operand instr 1 in
            acc @ [ mk_refute (NoAlias (u, v)) instr ]
          else if String.is_substring fname ~substring:__assert_may_alias
          then
            let u, v = LI.operand instr 0, LI.operand instr 1 in
            acc @ [ mk_assert (MayAlias (u, v)) instr ]
          else if String.is_substring fname ~substring:__refute_may_alias
          then
            let u, v = LI.operand instr 0, LI.operand instr 1 in
            acc @ [ mk_refute (MayAlias (u, v)) instr ]
          else if String.is_substring fname ~substring:__assert_must_alias
          then
            let u, v = LI.operand instr 0, LI.operand instr 1 in
            acc @ [ mk_assert (MustAlias (u, v)) instr ]
          else if String.is_substring fname ~substring:__refute_must_alias
          then
            let u, v = LI.operand instr 0, LI.operand instr 1 in
            acc @ [ mk_refute (MustAlias (u, v)) instr ]
          else acc
        | _ -> acc) in
  LI.deep_fold_func ~finstr [] func
;;

let find_range_assertions (func : LI.func) : assertion list =
  let visit_instr acc instr =
    let vinstr = LI.llvalue_of_instr instr in
    match LL.instr_opcode vinstr with
    | LO.Call ->
      let fname = LI.func_name (LI.callee_of_instr_call instr) in
      if String.is_substring fname ~substring:__assert_range_lower_bound
      then (
        let v, lb = LI.operand instr 0, LI.operand instr 1 in
        match LL.int64_of_const lb with
        | None -> acc
        | Some lb -> acc @ [ mk_assert (RangeLB (v, lb)) instr ])
      else if String.is_substring fname ~substring:__assert_range_upper_bound
      then (
        let v, ub = LI.operand instr 0, LI.operand instr 1 in
        match LL.int64_of_const ub with
        | None -> acc
        | Some ub -> acc @ [ mk_assert (RangeUB (v, ub)) instr ])
      else if String.is_substring fname ~substring:__assert_range_full
      then (
        let v = LI.operand instr 0 in
        let lb, ub = LI.operand instr 1, LI.operand instr 2 in
        match LL.int64_of_const lb, LL.int64_of_const ub with
        | Some lb, Some ub -> acc @ [ mk_assert (RangeLUB (v, lb, ub)) instr ]
        | _ -> acc)
      else acc
    | _ -> acc in
  LI.deep_fold_func ~finstr:(Some visit_instr) [] func
;;

let find_all_assertions (func : LI.func) : assertion list =
  find_alias_assertions func @ find_range_assertions func
;;

let count_all_assertions (prog : LI.program) : int =
  let assertions =
    List.fold_left
      ~f:(fun acc func -> acc @ find_all_assertions func)
      ~init:[] prog.prog_user_funcs in
  let _ = hprint "All assertions: " (pr_items ~f:pr_assertion) assertions in
  List.length assertions
;;
