(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Source
open Printer
open Llir
module LL = Llvm
module LD = Lldebug
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LA = List.Assoc

(*******************************************************************
 ** Data structure
 *******************************************************************)

(*-----------
 * Bug type
 *----------*)

type bug_type =
  | MemoryLeak of memory_leak
  | NullPointerDeref
  | BufferOverflow of buffer_overflow
  | IntegerOverflow of integer_overflow
  | IntegerUnderflow of integer_underflow
  | DivisionByZero

and integer_overflow =
  { iof_expr : llvalue;
    iof_bitwidth : int;
    iof_instr : instr
  }

and integer_underflow =
  { iuf_expr : llvalue;
    iuf_bitwidth : int;
    iuf_instr : instr
  }

and buffer_size =
  (* FIXME: need to change name of this variant type *)
  | NumElem of (int64 * lltype) (* number of element of type lltype *)
  | MemSizeOf of llvalue
(* size of allocated memory of pointer *)

and buffer_overflow =
  { bof_pointer : llvalue;
    bof_elem_index : llvalue;
    bof_buff_size : buffer_size;
    bof_instr : instr
  }

and memory_leak =
  { mlk_pointer : llvalue;
    mlk_size : int option
  }

(*------
 * Bug
 *-----*)

type bug =
  { bug_instr : instr;
    bug_func : func;
    bug_type : bug_type;
    bug_status : bool option;
    (* None means Unknown *)
    bug_analysis : string option;
    bug_reason : string option
  }

(*******************************************************************
 ** Printing
 *******************************************************************)

(*--------------
 * Memory bugs
 *-------------*)

let pr_buffer_overflow ?(detailed = true) (bof : buffer_overflow) : string =
  "Buffer Overflow"
  ^
  if detailed
  then
    "\n  Root pointer: "
    ^ sprint_value bof.bof_pointer
    ^ ", accessing index: "
    ^ sprint_value bof.bof_elem_index
  else ""
;;

let pr_memory_leak ?(detailed = true) (mlk : memory_leak) : string =
  let size =
    match mlk.mlk_size with
    | None -> "unknown"
    | Some size -> sprint_int size in
  "Memory Leak" ^ if detailed then "\n  Reason: buffer size: " ^ size else ""
;;

(*---------------
 * Integer bugs
 *--------------*)

let sprint_instr_detailed_position instr =
  let code_excerpt =
    match LD.position_of_instr instr with
    | None -> ""
    | Some p -> "  Location: " ^ sprint_file_position_and_excerpt p ^ "\n"
  in
  if !location_source_code_only
  then code_excerpt
  else "  Instruction: " ^ sprint_instr instr ^ "\n" ^ code_excerpt
;;

let pr_llvalue_name (v : LL.llvalue) : string =
  match LD.get_original_name_of_llvalue v with
  | Some str -> str
  | None -> sprint_value v
;;

let sprint_integer_overflow ?(detailed = true) (iof : integer_overflow)
    : string
  =
  "Integer Overflow"
  ^
  if detailed then "\n" ^ sprint_instr_detailed_position iof.iof_instr else ""
;;

let sprint_integer_underflow ?(detailed = true) (iuf : integer_underflow)
    : string
  =
  "Integer Underflow"
  ^
  if detailed then "\n" ^ sprint_instr_detailed_position iuf.iuf_instr else ""
;;

(*------------------------
 * Print bug information
 *-----------------------*)

let pr_bug_type ?(detailed = true) (btype : bug_type) : string =
  match btype with
  | MemoryLeak mlk -> pr_memory_leak mlk
  | NullPointerDeref -> "Null Pointer Dereference"
  | BufferOverflow bof -> pr_buffer_overflow ~detailed bof
  | IntegerOverflow iof -> sprint_integer_overflow iof
  | IntegerUnderflow iuf -> sprint_integer_underflow ~detailed iuf
  | DivisionByZero -> "Division By Zero"
;;

let pr_potential_bug (bug : bug) : string =
  (pr_bug_type ~detailed:false bug.bug_type ^ "\n")
  ^ ("    Function: " ^ func_name bug.bug_func ^ "\n")
  ^ "    "
  ^ sprint_instr bug.bug_instr
;;

let pr_potential_bugs (bugs : bug list) : string =
  hsprint_list_itemized ~f:pr_potential_bug bugs
;;

let pr_bug ?(detailed = true) (bug : bug) : string =
  let details =
    if detailed
    then (
      let status =
        match bug.bug_status with
        | None -> "Unknown"
        | Some b -> sprint_bool b in
      ("    Type: " ^ pr_bug_type ~detailed bug.bug_type)
      ^ "    Status: "
      ^ status)
    else "" in
  "Bug: "
  ^ sprint_instr bug.bug_instr
  ^ String.prefix_if_not_empty ~prefix:"\n" details
;;

let pr_bug_name (bug : bug) : string = pr_bug_type ~detailed:false bug.bug_type
let pr_bugs (bugs : bug list) : string = hsprint_list_itemized ~f:pr_bug bugs

(*******************************************************************
 ** constructors
 *******************************************************************)

(*-------------------------------------------
 * Potential bugs
 *------------------------------------------*)

let mk_potential_bug (instr : instr) (btype : bug_type) : bug =
  { bug_instr = instr;
    bug_func = func_of_instr instr;
    bug_type = btype;
    bug_status = None;
    bug_analysis = None;
    bug_reason = None
  }
;;

(*-------------------------------------------
 * Potential integer bugs
 *------------------------------------------*)

let mk_potential_integer_overflow (instr : instr) : bug =
  let expr = llvalue_of_instr instr in
  let iof =
    { iof_expr = expr;
      iof_bitwidth = LL.integer_bitwidth (LL.type_of expr);
      iof_instr = instr
    } in
  mk_potential_bug instr (IntegerOverflow iof)
;;

let mk_potential_integer_underflow (instr : instr) : bug =
  let expr = llvalue_of_instr instr in
  let iuf =
    { iuf_expr = expr;
      iuf_bitwidth = LL.integer_bitwidth (LL.type_of expr);
      iuf_instr = instr
    } in
  mk_potential_bug instr (IntegerUnderflow iuf)
;;

(*-------------------------------------------
 * Potential memory bugs
 *------------------------------------------*)

let mk_potential_buffer_overflow (instr : instr) : bug =
  let bof =
    match instr_opcode instr with
    | LO.GetElementPtr ->
      let ptr = src_of_instr_gep instr in
      let size, index =
        let elem_typ = LL.element_type (LL.type_of ptr) in
        let idxs = indexes_of_instr_gep instr in
        match LL.classify_type elem_typ with
        (* pointer to an array *)
        | LL.TypeKind.Array ->
          let size = Int64.of_int (LL.array_length elem_typ) in
          let array_idx =
            match List.nth idxs 1 with
            | Some idx -> idx
            | None ->
              herror
                "mk_potential_buffer_overflow: array index not available:"
                sprint_instr
                instr in
          NumElem (size, elem_typ), array_idx
        (* pointer to a dynamically allocated memory *)
        | _ -> MemSizeOf ptr, List.hd_exn idxs in
      { bof_instr = instr;
        bof_pointer = ptr;
        bof_buff_size = size;
        bof_elem_index = index
      }
    | _ -> error "mk_buffer_overflow: expect GetElementPtr" in
  mk_potential_bug instr (BufferOverflow bof)
;;

let mk_potential_memory_leak (instr : instr) : bug =
  let mlk = { mlk_pointer = llvalue_of_instr instr; mlk_size = None } in
  mk_potential_bug instr (MemoryLeak mlk)
;;

(*-------------------------------------------
 * Real bugs
 *------------------------------------------*)

let mk_real_bug ~(reason : string) ~(analysis : string) (bug : bug) : bug =
  { bug with
    bug_status = Some true;
    bug_analysis = Some analysis;
    bug_reason = Some reason
  }
;;

(*******************************************************************
 ** reporting
 *******************************************************************)

let report_bug (bug : bug) : unit =
  let location =
    match !llvm_orig_source_name with
    | false -> ""
    | true ->
      (match LD.position_of_instr bug.bug_instr with
      | None -> ""
      | Some p -> "  " ^ sprint_file_position_and_excerpt p ^ "\n") in
  let reason =
    match bug.bug_reason with
    | None -> ""
    | Some s -> String.indent_line 2 ("Reason: " ^ s) ^ "\n" in
  let msg =
    "BUG: "
    ^ pr_bug_type ~detailed:false bug.bug_type
    ^ "\n"
    ^ location
    ^ reason in
  print_endline ("\n" ^ msg)
;;

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
          acc ^ "\n  " ^ bug_name ^ ": " ^ sprint_int times)
        ~init:""
        bug_stats in
  print ("==============================\n" ^ "Bug Summary:\n" ^ summary)
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

let annotate_potential_bugs (prog : program) : bug list =
  let finstr =
    Some
      (fun acc instr ->
        match instr_opcode instr with
        | LO.Add | LO.Sub | LO.Mul | LO.SDiv | LO.UDiv ->
          acc
          @ [ mk_potential_integer_overflow instr;
              mk_potential_integer_underflow instr
            ]
        | LO.GetElementPtr -> acc @ [ mk_potential_buffer_overflow instr ]
        | LO.Ret -> acc @ [ mk_potential_memory_leak instr ]
        | _ -> acc) in
  let funcs = prog.prog_user_funcs in
  List.fold_left
    ~f:(fun acc func -> acc @ deep_fold_func ~finstr [] func)
    ~init:[]
    funcs
;;
