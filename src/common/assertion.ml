(******************************************************************
 ** Author: Ta Quang Trung
 ** Date: 2020
 ******************************************************************)


open Core
open Dcore

module LL = Llvm
module LI = Llir
module LS = Llsrc
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LA = List.Assoc

(*******************************************************************
 ** data structure
 *******************************************************************)

type predicate =
  (* alias *)
  | NoAlias of (LL.llvalue list)
  | MustAlias of (LL.llvalue list)
  | MayAlias of (LL.llvalue list)
  (* range *)
  | RangeLB of (LL.llvalue list)
  | RangeUB of (LL.llvalue list)
  | RangeFull of (LL.llvalue list)
  (* interger *)
  | IntegerOverflow
  | IntegerUnderflow
  (* buffer *)
  | BufferOverflow

type assertion_type =
  | Assert
  | Refute

type assertion = {
  ast_instr : LI.instr;
  ast_predicate : predicate;
  ast_type : assertion_type;
}

(*******************************************************************
 ** printing
 *******************************************************************)

let pr_predicate (pred: predicate) : string =
  match pred with
  | NoAlias vs ->
    "NoAlias(" ^ (pr_args LI.pr_value vs) ^ ")"
  | MayAlias vs ->
    "MayAlias(" ^ (pr_args LI.pr_value vs) ^ ")"
  | MustAlias vs ->
    "MustAlias(" ^ (pr_args LI.pr_value vs) ^ ")"
  | _ -> "pr_predicate: to implement"

let pr_assertion (ast: assertion) : string =
  LI.pr_instr ast.ast_instr

let pr_assertion_status (func: LI.func) (ast: assertion) (status: bool) =
  let instr = ast.ast_instr in
  let fname = LI.func_name func in
  let assertion = match LI.is_instr_call_or_invoke instr with
    | false -> herror "assertion must be a function call: " LI.pr_instr instr
    | true ->
      let asname = LI.func_name (LI.callee_of_instr_call_or_invoke instr) in
      let args = LI.args_of_instr_call_or_invoke instr in
      asname ^ "(" ^ (pr_args LI.pr_value args) ^ ")" in
  let location =
    try
      let loc = LS.location_of_instr instr in
      let file = get_file_name_of_location loc in
      let line, _ = get_line_numbers_of_location loc in
      "File: " ^ file ^ ", function: " ^ fname ^ ", line: " ^ (pr_int line)
    with _ -> "Function: " ^ fname in
  location ^ "\n  " ^ assertion ^
  (if status then ": OK!" else ": FAILED!")

(*******************************************************************
 ** constructors
 *******************************************************************)

let mk_assertion assertion_type predicate (instr: LI.instr) =
  { ast_instr = instr;
    ast_predicate = predicate;
    ast_type = assertion_type; }

let find_alias_assertions (func: LI.func) : assertion list =
  let finstr = Some (fun acc instr ->
    let vinstr = LI.llvalue_of_instr instr in
    match LL.instr_opcode vinstr with
    | LO.Call | LO.Invoke ->
      let callee = LI.callee_of_instr_call_or_invoke instr in
      let fname = LI.func_name callee in
      let operands = LI.operands instr in
      if String.is_substring fname ~substring:__assert_no_alias then
        acc @ [mk_assertion Assert (NoAlias operands) instr]
      else if String.is_substring fname ~substring:__refute_no_alias then
        acc @ [mk_assertion Refute (NoAlias operands) instr]
      else if String.is_substring fname ~substring:__assert_may_alias then
        acc @ [mk_assertion Assert (MayAlias operands) instr]
      else if String.is_substring fname ~substring:__refute_may_alias then
        acc @ [mk_assertion Refute (MayAlias operands) instr]
      else if String.is_substring fname ~substring:__assert_must_alias then
        acc @ [mk_assertion Assert (MustAlias operands) instr]
      else if String.is_substring fname ~substring:__refute_must_alias then
        acc @ [mk_assertion Refute (MustAlias operands) instr]
      else acc
    | _ -> acc) in
  LI.deep_fold_func ~finstr [] func

let find_range_assertions (func: LI.func) : assertion list =
  let finstr = Some (fun acc instr ->
    let vinstr = LI.llvalue_of_instr instr in
    match LL.instr_opcode vinstr with
    | LO.Call ->
      let fname = LI.func_name (LI.callee_of_instr_call instr) in
      let operands = LI.operands instr in
      if (String.is_substring fname ~substring:__assert_range_lower_bound) then
        acc @ [mk_assertion Assert (RangeLB operands) instr]
      else if String.is_substring fname ~substring:__assert_range_upper_bound then
        acc @ [mk_assertion Assert (RangeUB operands) instr]
      else if String.is_substring fname ~substring:__assert_range_full then
        acc @ [mk_assertion Assert (RangeFull operands) instr]
      else acc
    | _ -> acc) in
  LI.deep_fold_func ~finstr [] func


let find_all_assertions (func: LI.func) : assertion list =
  (find_alias_assertions func) @ (find_range_assertions func)

let count_all_assertions (prog: LI.program) : int =
  let assertions = List.fold_left ~f:(fun acc func ->
    acc @ (find_all_assertions func)) ~init:[] prog.prog_user_funcs in
  let _ = hprint "All assertions: " (pr_items pr_assertion) assertions in
  List.length assertions
