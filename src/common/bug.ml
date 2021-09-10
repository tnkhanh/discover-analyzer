(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore
open Llir

module LL = Llvm
module LS = Llsrc
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LA = List.Assoc

(*******************************************************************
 ** data structure
 *******************************************************************)

(* bug type *)

type bug_type =
  | MemoryLeak of memory_leak
  | NullPointerDeref
  | BufferOverflow of buffer_overflow
  | IntegerOverflow
  | IntegerUnderflow
  | DivisionByZero

and buffer_overflow = {
  bof_instr : instr;
  bof_pointer : llvalue;
  bof_size : int option;
  bof_index : llvalue;
}

and memory_leak = {
  mlk_pointer : llvalue;
  mlk_size : int option;
}

(* bug *)

type bug = {
  bug_instr : instr;
  bug_func : func;
  bug_type : bug_type;
  bug_status : ternary;          (* True, False, or Unknown *)
  bug_analyzer : string option;
  bug_reason : string option;
}

(*******************************************************************
 ** printing
 *******************************************************************)

let pr_buffer_overflow (bof: buffer_overflow) : string =
  let size = match bof.bof_size with
    | None -> "unknown"
    | Some size -> pr_int size in
  "BUFFER OVERFLOW\n" ^
  "  Reason: buffer size: " ^ size ^
  ", accessing index: " ^ (pr_value bof.bof_index)

let pr_memory_leak (mlk: memory_leak) : string =
  let size = match mlk.mlk_size with
    | None -> "unknown"
    | Some size -> pr_int size in
  "MEMORY LEAK\n" ^
  "  Reason: buffer size: " ^ size

let pr_bug_type_detail (btype: bug_type) : string =
  match btype with
  | MemoryLeak mlk -> pr_memory_leak mlk
  | NullPointerDeref -> "NULL POINTER DEREFERENCE"
  | BufferOverflow bof -> pr_buffer_overflow bof
  | IntegerOverflow -> "INTEGER OVERFLOW"
  | IntegerUnderflow -> "INTEGER UNDERFLOW"
  | DivisionByZero -> "DIVISION BY ZERO"

let pr_bug_type_summary (btype: bug_type) : string =
  match btype with
  | MemoryLeak _ -> "Memory Leak"
  | NullPointerDeref -> "Null Pointer Dereference"
  | BufferOverflow _ -> "Buffer Overflow"
  | IntegerOverflow -> "Integer Overflow"
  | IntegerUnderflow -> "Integer Underflow"
  | DivisionByZero -> "Division By Zero"

let pr_potential_bug (bug: bug) : string =
  (pr_bug_type_summary bug.bug_type) ^ "\n" ^
  "    Function: " ^ (func_name bug.bug_func) ^ "\n" ^
  "    " ^ (pr_instr bug.bug_instr)

let pr_potential_bugs (bugs: bug list) : string =
  pr_items pr_potential_bug bugs

let pr_bug (bug: bug) : string =
  "Bug: " ^ (pr_instr bug.bug_instr) ^ "\n" ^
  "    Type: " ^ (pr_bug_type_detail bug.bug_type) ^
  "    Status: " ^ pr_ternary bug.bug_status

let pr_bugs (bugs: bug list) : string =
  pr_items pr_bug bugs

let pr_bug_summary (bug: bug) : string =
  pr_bug_type_summary bug.bug_type

(*******************************************************************
 ** constructors
 *******************************************************************)

let mk_potential_bug (instr: instr) (btype: bug_type) : bug =
  { bug_instr = instr;
    bug_func = func_of_instr instr;
    bug_type = btype;
    bug_status = Unkn;
    bug_analyzer = None;
    bug_reason = None; }

(* let mk_potential_buffer_overflow (instr: instr) : bug =
 *   let bof = match instr_opcode instr with
 *     | LO.GetElementPtr ->
 *       let ptr = src_of_instr_gep instr in
 *       let size, index =
 *         let elem_typ = LL.element_type (LL.type_of ptr) in
 *         match LL.classify_type elem_typ with
 *         (\* pointer to an array *\)
 *         | LL.TypeKind.Array ->
 *           let size = LL.array_length elem_typ in
 *           let index = match second_index_of_instr_gep instr with
 *             | Some idx -> idx
 *             | None ->
 *               herror "mk_potential_buffer_overflow: second index unavailable:"
 *                 pr_instr instr in
 *           (Some size, index)
 *         (\* pointer to a dynamically allocated memory *\)
 *         | _ ->
 *           let index = first_index_of_instr_gep instr in
 *           (None, index) in
 *       { bof_instr = instr;
 *         bof_pointer = ptr;
 *         bof_size = size;
 *         bof_index = index; }
 *     | _ -> error "mk_buffer_overflow: expect GetElementPtr" in
 *   mk_potential_bug instr (BufferOverflow bof) *)

let mk_potential_memory_leak (instr: instr) : bug =
  let mlk = { mlk_pointer = llvalue_of_instr instr;
              mlk_size = None; } in
  mk_potential_bug instr (MemoryLeak mlk)

let mk_real_bug ?(reason=None) (analyzer: string) (bug: bug) : bug =
  { bug with
    bug_status = True;
    bug_analyzer = Some analyzer;
    bug_reason = reason; }

(*******************************************************************
 ** reporting
 *******************************************************************)

let report_bug (bug: bug) : unit =
  let location = match !llvm_orig_source_name with
    | false -> ""
    | true ->
      let loc = LS.location_of_instr bug.bug_instr in
      " Location: at " ^ (pr_location loc) ^ "\n" in
  let msg = "BUG: " ^ (pr_bug_type_detail bug.bug_type) ^ "\n" ^ location in
  print_endline ("\n" ^ msg)

let report_bug_stats (bugs: bug list) : unit =
  match bugs with
  | [] -> print "No bug is detected!"
  | _ ->
    let tbl_stats = Hashtbl.create (module String) in
    let _ = List.iter ~f:(fun bug ->
      let bug_name = pr_bug_summary bug in
      let times = match Hashtbl.find tbl_stats bug_name with
        | Some n -> n + 1
        | None -> 1 in
      Hashtbl.set tbl_stats ~key:bug_name ~data:times) bugs in
    let bug_stats = Hashtbl.to_alist tbl_stats in
    let bug_info = List.fold_left ~f:(fun acc (bug_name, times) ->
      acc ^ "\n  " ^ bug_name ^ ": " ^ (string_of_int times)
    ) ~init:"Bug Summary:\n" bug_stats in
    print bug_info


(*******************************************************************
 ** utilities
 *******************************************************************)

(* TODO:
   restructure bugs mechanism by a hashing table from
      function --> a list of bugs
*)

let annotate_potential_bugs (prog: program) : bug list =
  let finstr = Some (fun acc instr ->
    match instr_opcode instr with
    (* | LO.GetElementPtr -> acc @ [mk_potential_buffer_overflow instr] *)
    | LO.Ret -> acc @ [mk_potential_memory_leak instr]
    | _ -> acc) in
  let funcs = prog.prog_user_funcs in
  List.fold_left ~f:(fun acc func ->
    acc @ (deep_fold_func ~finstr [] func)) ~init:[] funcs
