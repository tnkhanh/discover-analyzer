(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Source
open Llir
module LL = Llvm
module LO = Llvm.Opcode

(*******************************************************************
 ** Data structure
 *******************************************************************)

(*------------------------------------------------------
 * Bug type, used by bug detection and instrumentation
 *-----------------------------------------------------*)

(** Module containing definitions of arithmetic bugs *)

module ArithmeticBug = struct
  type integer_overflow =
    { iof_expr : value;
      iof_bitwidth : int;
      iof_instr : instr
    }

  type integer_underflow =
    { iuf_expr : value;
      iuf_bitwidth : int;
      iuf_instr : instr
    }

  type integer_coercion_error =
    { ice_expr : value;
      ice_instr : instr
    }

  type numeric_truncation_error =
    { nte_expr : value;
      nte_instr : instr
    }

  type division_by_zero =
    { dbz_expr : value;
      dbz_instr : instr
    }
end

(** Module containing definitions of memory bugs *)

module MemoryBug = struct
  (*---------------
   * Data structures
   *--------------*)

  type memory_leak =
    { mlk_pointer : value;
      mlk_size : int option
    }

  type null_pointer_deref = { npe_pointer : value }

  type buffer_overflow =
    { bof_pointer : value;
      bof_elem_index : value;
      bof_instr : instr;
      mutable bof_write_operation : bool option;
      mutable bof_stack_based : bool option
    }
end

(** Module containing definitions of resource bugs *)

module ResourceBug = struct
  type resource_leak =
    { rlk_pointer : value;
      rlk_file_resource : bool
    }
end

(** Module containing definitions of memory bugs *)

module SolidityBug = struct
  (* Example of access control bugs like:
     - Authorization using `tx.origin`, e.g.: `require(tx.origin == owner)`
  *)
  type solidity_access_control = { sac_pointer : value }
end

(* Include all modules *)

include ArithmeticBug
include MemoryBug
include ResourceBug
include SolidityBug

(* Define bug types *)
type bug_type =
  (* Numerical bugs *)
  | IntegerOverflow of integer_overflow option
  | IntegerUnderflow of integer_underflow option
  | IntegerCoercionError of integer_coercion_error option
  | NumericTruncationError of numeric_truncation_error option
  | DivisionByZero of division_by_zero option
  (* Memory bugs *)
  | MemoryLeak of memory_leak option
  | NullPointerDeref of null_pointer_deref option
  | BufferOverflow of buffer_overflow option
  (* Resources bugs *)
  | ResourceLeak of resource_leak option
  (* Soldity bugs *)
  | SolidityAccessControl of solidity_access_control

type potential_bug =
  { pbug_instr : instr;
    pbug_func : func;
    pbug_type : bug_type
  }

type bug =
  { bug_instr : instr;
    bug_func : func;
    bug_type : bug_type;
    bug_checker : string;
    bug_reason : string
  }

type bug_types = bug_type list
type potential_bugs = potential_bug list
type bugs = bug list

(*******************************************************************
 ** Printing
 *******************************************************************)

(*------------------------
 * Print bug information
 *-----------------------*)

let pr_bug_cwe (btype : bug_type) : string =
  match btype with
  (* Integer bugs *)
  | IntegerOverflow _ -> "CWE-190: Integer Overflow"
  | IntegerUnderflow _ -> "CWE-191: Integer Underflow"
  | IntegerCoercionError _ -> "CWE-192: Integer Coercion Error"
  | NumericTruncationError _ -> "CWE-197: Numeric Truncation Error"
  | DivisionByZero _ -> "CWE-369: Divide By Zero"
  (* Memory bugs *)
  | MemoryLeak _ ->
    "CWE-401 -- Missing Release of Memory after Effective Lifetime"
  | NullPointerDeref _ -> "CWE-476: NULL Pointer Dereference"
  | BufferOverflow bof_opt ->
    (match bof_opt with
    | None -> "CWE-805: Buffer Access with Incorrect Length Value"
    | Some bof ->
      (match bof.bof_write_operation with
      | Some true ->
        (match bof.bof_stack_based with
        | Some true -> "CWE-121: Stack-based Buffer Overflow"
        | Some false -> "CWE-122: Heap-based Buffer Overflow"
        | None ->
          "CWE-???: Buffer Overflow -- "
          ^ "unable to determine whether the buffer is stack- or heap-based")
      | Some false -> "CWE-125: Out-of-bounds Read"
      | None ->
        "CWE-???: Buffer Overflow -- "
        ^ "unable to determine whether the buffer is read or written"))
  (* Resource bugs *)
  | ResourceLeak rlk_opt ->
    (match rlk_opt with
    | None -> "CWE-772: Missing Release of Resource after Effective Lifetime"
    | Some rlk ->
      if rlk.rlk_file_resource
      then
        "CWE-775: Missing Release of File Descriptor or Handle"
        ^ " after Effective Lifetime"
      else "CWE-772: Missing Release of Resource after Effective Lifetime")
  (* Solidity bugs: TODO: check with Solidity CWE *)
  | SolidityAccessControl _ -> ""
;;

let pr_bug_type (btype : bug_type) : string =
  match btype with
  (* Integer bugs *)
  | IntegerOverflow _ -> "Integer Overflow"
  | IntegerUnderflow _ -> "Integer Underflow"
  | IntegerCoercionError _ -> "Integer Coercion Error"
  | NumericTruncationError _ -> "Numeric Truncation Error"
  | DivisionByZero _ -> "Division By Zero"
  (* Memory bugs *)
  | MemoryLeak _ -> "Memory Leak"
  | NullPointerDeref _ -> "Null Pointer Dereference"
  | BufferOverflow _ -> "Buffer Overflow"
  (* Resource bugs *)
  | ResourceLeak _ -> "Resource Leak"
  | SolidityAccessControl _ -> "Solidity Access Control"
;;

let pr_bug_type_lowercase (btype : bug_type) : string =
  match btype with
  (* Integer bugs *)
  | IntegerOverflow _ -> "integer_over_under_flow"
  | IntegerUnderflow _ -> "integer_over_under_flow"
  | IntegerCoercionError _ -> "integer_coercion_error"
  | NumericTruncationError _ -> "numeric_truncation_error"
  | DivisionByZero _ -> "division_by_zero"
  (* Memory bugs *)
  | MemoryLeak _ -> "memory_leak"
  | NullPointerDeref _ -> "null_pointer_dereference"
  | BufferOverflow _ -> "buffer_overflow"
  (* Resource bugs *)
  | ResourceLeak _ -> "resource_leak"
  (* Solidity bugs *)
  | SolidityAccessControl _ -> "solidity_access_control"
;;

let pr_potential_bug (pbug : potential_bug) : string =
  "Potential "
  ^ (pr_bug_type pbug.pbug_type ^ " bug at:\n")
  ^ sprintf "+ instruction:\n  %s\n" (pr_instr pbug.pbug_instr)
  ^ sprintf "+ function: %s\n" (func_name pbug.pbug_func)
;;

let pr_potential_bugs (pbugs : potential_bug list) : string =
  pr_items ~f:pr_potential_bug pbugs
;;

let pr_bug (bug : bug) : string =
  let bug_type_info =
    let cwe = pr_bug_cwe bug.bug_type in
    pr_bug_type bug.bug_type
    ^ String.surround_if_not_empty ~prefix:" (" ~suffix:")" cwe
    ^ "\n" in
  let instr = sprintf "LLVM instruction:\n  %s\n" (pr_instr bug.bug_instr) in
  let code_excerpt =
    if !report_source_code_name
    then (
      match position_of_instr bug.bug_instr with
      | None -> ""
      | Some p ->
        let fname = func_name bug.bug_func in
        pr_code_excerpt_and_location p ~func:fname ^ "\n")
    else "" in
  let reason = String.align_line "Reason: " bug.bug_reason in
  "BUG: " ^ bug_type_info ^ String.indent 2 (instr ^ code_excerpt ^ reason)
;;

let pr_bug_name (bug : bug) : string = pr_bug_type bug.bug_type

let pr_bugs (bugs : bug list) : string =
  pr_list ~obrace:"\n" ~cbrace:"" ~sep:"\n\n" ~f:pr_bug bugs
;;

(*******************************************************************
 ** constructors
 *******************************************************************)

(*** memory bugs ***)

let mk_bug_type_memory_leak () : bug_type = MemoryLeak None
let mk_bug_type_null_pointer_deref () : bug_type = NullPointerDeref None
let mk_bug_type_buffer_overflow () : bug_type = BufferOverflow None

(*** integer bugs ***)

let mk_bug_type_integer_overflow () : bug_type = IntegerOverflow None
let mk_bug_type_integer_underflow () : bug_type = IntegerUnderflow None
let mk_bug_type_coercion_error () : bug_type = IntegerCoercionError None
let mk_bug_type_truncation_error () : bug_type = NumericTruncationError None
let mk_bug_type_division_by_zero () : bug_type = DivisionByZero None

(*-------------------------------------------
 * Potential bugs
 *------------------------------------------*)

let mk_potential_bug (ins : instr) (btype : bug_type) : potential_bug =
  { pbug_instr = ins; pbug_func = func_of_instr ins; pbug_type = btype }
;;

(*-------------------------------------------
 * Potential integer bugs
 *------------------------------------------*)

let mk_potential_integer_overflow (ins : instr) : potential_bugs =
  if !bug_all || !bug_integer_all || !bug_integer_overflow
  then (
    let expr = llvalue_of_instr ins in
    let iof =
      { iof_expr = expr;
        iof_bitwidth = LL.integer_bitwidth (LL.type_of expr);
        iof_instr = ins
      } in
    [ mk_potential_bug ins (IntegerOverflow (Some iof)) ])
  else []
;;

let mk_potential_integer_underflow (ins : instr) : potential_bugs =
  if !bug_all || !bug_integer_all || !bug_integer_underflow
  then (
    let expr = llvalue_of_instr ins in
    let iuf =
      { iuf_expr = expr;
        iuf_bitwidth = LL.integer_bitwidth (LL.type_of expr);
        iuf_instr = ins
      } in
    [ mk_potential_bug ins (IntegerUnderflow (Some iuf)) ])
  else []
;;

let mk_potential_division_by_zero (ins : instr) : potential_bugs =
  if !bug_all || !bug_integer_all || !bug_divizion_by_zero
  then (
    let dbz = { dbz_expr = llvalue_of_instr ins; dbz_instr = ins } in
    [ mk_potential_bug ins (DivisionByZero (Some dbz)) ])
  else []
;;

(*-------------------------------------------
 * Potential memory bugs
 *------------------------------------------*)

(* let compute_buffer_accessing_index () *)

let mk_potential_buffer_overflow (ins : instr) : potential_bugs =
  if !bug_all || !bug_memory_all || !bug_buffer_overflow
  then (
    let bof =
      match instr_opcode ins with
      | LO.GetElementPtr ->
        let ptr = src_of_instr_gep ins in
        let index =
          let elem_typ = LL.element_type (LL.type_of ptr) in
          let idxs = indexes_of_instr_gep ins in
          match LL.classify_type elem_typ with
          (* pointer to an array *)
          | LL.TypeKind.Array ->
            (match List.nth idxs 1 with
            | Some idx -> idx
            | None ->
              errorp "mk_potential_buffer_overflow: array index not available:"
                pr_instr ins)
          (* pointer to a dynamically allocated memory *)
          | _ -> List.hd_exn idxs in
        { bof_instr = ins;
          bof_pointer = ptr;
          bof_elem_index = index;
          bof_write_operation = Some false;
          bof_stack_based = None
        }
      | LO.Call | LO.Invoke ->
        let dst_ptr = operand ins 0 in
        let callee = callee_of_instr_func_call ins in
        if is_func_llvm_memcpy callee || is_func_llvm_memmove callee
        then (
          let index = operand ins 2 in
          { bof_instr = ins;
            bof_pointer = dst_ptr;
            bof_elem_index = index;
            bof_write_operation = Some true;
            bof_stack_based = None
          })
        else errorp "mk_buffer_overflow: need to hande: " pr_instr ins
      | _ -> errorp "mk_buffer_overflow: need to hande: " pr_instr ins in
    [ mk_potential_bug ins (BufferOverflow (Some bof)) ])
  else []
;;

let mk_potential_memory_leak (ins : instr) : potential_bugs =
  if !bug_all || !bug_memory_all || !bug_memory_leak
  then (
    let mlk = { mlk_pointer = llvalue_of_instr ins; mlk_size = None } in
    [ mk_potential_bug ins (MemoryLeak (Some mlk)) ])
  else []
;;

(*-------------------------------------------
 * Real bugs
 *------------------------------------------*)

let mk_real_bug ~(reason : string) ~(checker : string) (pbug : potential_bug)
    : bug
  =
  { bug_instr = pbug.pbug_instr;
    bug_func = pbug.pbug_func;
    bug_type = pbug.pbug_type;
    bug_checker = checker;
    bug_reason = reason
  }
;;

(*******************************************************************
 ** reporting
 *******************************************************************)

let report_bug_stats (bugs : bug list) : unit =
  let summary =
    match bugs with
    | [] -> "\n  No bug is detected!"
    | _ ->
      let tbl_stats = Hashtbl.create (module String) in
      let _ =
        List.iter
          ~f:(fun bug ->
            let bug_name = pr_bug_name bug in
            let times =
              match Hashtbl.find tbl_stats bug_name with
              | Some n -> n + 1
              | None -> 1 in
            Hashtbl.set tbl_stats ~key:bug_name ~data:times)
          bugs in
      let bug_stats = Hashtbl.to_alist tbl_stats in
      List.fold_left
        ~f:(fun acc (bug_name, times) ->
          acc ^ "\n  " ^ bug_name ^ ": " ^ pr_int times)
        ~init:"" bug_stats in
  print ~mtype:"" ~ruler:`Medium ("Bug Summary:\n" ^ summary)
;;

(*******************************************************************
 ** utilities
 *******************************************************************)

let is_bug_buffer_overflow (bug : bug) =
  match bug.bug_type with
  | BufferOverflow _ -> true
  | _ -> false
;;

let is_bug_memory_leak (bug : bug) =
  match bug.bug_type with
  | MemoryLeak _ -> true
  | _ -> false
;;

let is_bug_integer_overflow (bug : bug) =
  match bug.bug_type with
  | IntegerOverflow _ -> true
  | _ -> false
;;

let is_bug_integer_underflow (bug : bug) =
  match bug.bug_type with
  | IntegerUnderflow _ -> true
  | _ -> false
;;

(* TODO:
   restructure bugs mechanism by a hashing table from
      function --> a list of bugs
*)

let record_potential_bugs (prog : program) : potential_bugs =
  let process_instr acc (ins : instr) =
    match instr_opcode ins with
    | LO.Add | LO.Sub | LO.Mul ->
      acc
      @ mk_potential_integer_overflow ins
      @ mk_potential_integer_underflow ins
    | LO.SDiv | LO.UDiv ->
      acc
      @ mk_potential_integer_overflow ins
      @ mk_potential_integer_underflow ins
      @ mk_potential_division_by_zero ins
    | LO.GetElementPtr -> acc @ mk_potential_buffer_overflow ins
    | LO.Call | LO.Invoke ->
      let callee = callee_of_instr_func_call ins in
      if is_func_llvm_memcpy callee || is_func_llvm_memmove callee
      then acc @ mk_potential_buffer_overflow ins
      else acc
    | LO.Ret -> acc @ mk_potential_memory_leak ins
    | _ -> acc in
  let funcs = prog.prog_user_funcs in
  List.fold_left
    ~f:(fun acc func ->
      acc @ visit_fold_func ~finstr:(Some process_instr) [] func)
    ~init:[] funcs
;;
