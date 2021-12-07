(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Source
open Llir
module LL = Llvm
module LD = Lldebug
module LO = Llvm.Opcode

(*******************************************************************
 ** Data structure
 *******************************************************************)

(*------------------------------------------------------
 * Bug type, used by bug detection and instrumentation
 *-----------------------------------------------------*)

type bug_type =
  (* memory bugs *)
  | MemoryLeak of memory_leak option
  | NullPointerDeref of null_pointer_deref option
  | BufferOverflow of buffer_overflow option
  (* numerical bugs *)
  | IntegerOverflow of integer_overflow option
  | IntegerUnderflow of integer_underflow option
  | IntegerCoercionError of integer_coercion_error option
  | NumericTruncationError of numeric_truncation_error option
  | DivisionByZero of division_by_zero option

(*** Memory bug informations ***)
and memory_leak =
  { mlk_pointer : llvalue;
    mlk_size : int option
  }

and null_pointer_deref = { npe_pointer : llvalue }

and buffer_size =
  (* FIXME: need to change name of this variant type *)
  | NumElem of (int64 * lltype) (* number of element of type lltype *)
  | MemSizeOf of llvalue
(* size of allocated memory of pointer *)

and buffer_overflow =
  { bof_pointer : llvalue;
    bof_elem_index : llvalue;
    bof_buff_size : buffer_size;
    bof_write_operation : bool;
    bof_stack_based : bool;
    bof_instr : instr
  }

(*** Numerical bug information ***)

(* Integer Overflow: https://cwe.mitre.org/data/definitions/190.html *)
and integer_overflow =
  { iof_expr : llvalue;
    iof_bitwidth : int;
    iof_instr : instr
  }

(* Integer Underflow: https://cwe.mitre.org/data/definitions/191.html *)
and integer_underflow =
  { iuf_expr : llvalue;
    iuf_bitwidth : int;
    iuf_instr : instr
  }

(* Integer Coercion Error: https://cwe.mitre.org/data/definitions/192.html *)
(* TODO: fill more details later *)
and integer_coercion_error =
  { ice_expr : llvalue;
    ice_instr : instr
  }

(* Numeric Truncation Error: https://cwe.mitre.org/data/definitions/197.html *)
and numeric_truncation_error =
  { nte_expr : llvalue;
    nte_instr : instr
  }

(* Division by Zero: https://cwe.mitre.org/data/definitions/369.html *)
and division_by_zero =
  { dbz_expr : llvalue;
    dbz_instr : instr
  }

type bug_types = bug_type list

(*------
 * Bug
 *-----*)

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

type potential_bugs = potential_bug list
type bugs = bug list

(*******************************************************************
 ** Printing
 *******************************************************************)

let pr_instr_location_and_code_excerpt instr =
  let code_excerpt =
    match LD.position_of_instr instr with
    | None -> ""
    | Some p -> "  Location: " ^ pr_file_position_and_excerpt p ^ "\n" in
  if !location_source_code_only
  then code_excerpt
  else
    "  Instruction: " ^ pr_instr instr
    ^ String.prefix_if_not_empty ~prefix:"\n" code_excerpt
;;

(*--------------
 * Memory bugs
 *-------------*)

let pr_buffer_overflow_info (bof : buffer_overflow) : string =
  sprintf "Root pointer: %s, " (pr_value bof.bof_pointer)
  ^ sprintf "accessing index: %s" (pr_value bof.bof_elem_index)
;;

let pr_memory_leak_info (mlk : memory_leak) : string =
  let size =
    match mlk.mlk_size with
    | None -> "unknown"
    | Some size -> pr_int size in
  "Buffer of size " ^ size ^ " is not freed when the program exits."
;;

(*---------------
 * Integer bugs
 *--------------*)

let pr_llvalue_name (v : LL.llvalue) : string =
  match LD.get_original_name_of_llvalue v with
  | Some str -> str
  | None -> pr_value v
;;

let pr_integer_overflow_info ?(detailed = true) (iof : integer_overflow)
    : string
  =
  let bug_info = "Integer Overflow" in
  if detailed
  then bug_info ^ "\n" ^ pr_instr_location_and_code_excerpt iof.iof_instr
  else bug_info
;;

let pr_integer_underflow_info ?(detailed = true) (iuf : integer_underflow)
    : string
  =
  let bug_info = "Integer Underflow" in
  if detailed
  then bug_info ^ "\n" ^ pr_instr_location_and_code_excerpt iuf.iuf_instr
  else bug_info
;;

(*------------------------
 * Print bug information
 *-----------------------*)

let pr_bug_cwe (btype : bug_type) : string =
  match btype with
  | MemoryLeak _ -> ""
  | NullPointerDeref _ -> "CWE-??? (npe)"
  (* FIXME: Khanh, please help to determine CWE-number based on this
     classification: https://cwe.mitre.org/data/definitions/787.html *)
  | BufferOverflow _ -> "CWE-???"
  | IntegerOverflow _ -> "CWE-190"
  | IntegerUnderflow _ -> "CWE-191"
  | IntegerCoercionError _ -> "CWE-192"
  | NumericTruncationError _ -> "CWE-197"
  | DivisionByZero _ -> "CWE-369"
;;

let pr_bug_type ?(detailed = true) (btype : bug_type) : string =
  match btype with
  | MemoryLeak _ -> "Memory Leak"
  | NullPointerDeref _ -> "Null Pointer Dereference"
  | BufferOverflow _ -> "Buffer Overflow"
  | IntegerOverflow _ -> "Integer Overflow"
  | IntegerUnderflow _ -> "Integer Underflow"
  | IntegerCoercionError _ -> "Integer Coercion Error"
  | NumericTruncationError _ -> "Numeric Truncation Error"
  | DivisionByZero _ -> "Division By Zero"
;;

let pr_potential_bug (pbug : potential_bug) : string =
  (pr_bug_type ~detailed:false pbug.pbug_type ^ "\n")
  ^ sprintf "  Instruction: %s\n" (pr_instr pbug.pbug_instr)
  ^ sprintf "  Function: %s\n" (func_name pbug.pbug_func)
;;

let pr_potential_bugs (pbugs : potential_bug list) : string =
  pr_items ~f:pr_potential_bug pbugs
;;

let pr_bug (bug : bug) : string =
  let bug_type_info =
    let btype = pr_bug_type bug.bug_type in
    let cwe = pr_bug_cwe bug.bug_type in
    btype ^ String.surround_if_not_empty ~prefix:" (" ~suffix:")" cwe in
  let location =
    match !llvm_orig_source_name with
    | false -> ""
    | true ->
      (match LD.position_of_instr bug.bug_instr with
      | None -> ""
      | Some p -> "  " ^ pr_file_position_and_excerpt p ^ "\n") in
  "BUG: " ^ bug_type_info ^ "\n" ^ location
  ^ String.indent 2 (String.align_line "Reason: " bug.bug_reason)
;;

let pr_bug_name (bug : bug) : string = pr_bug_type ~detailed:false bug.bug_type
let pr_bugs (bugs : bug list) : string = pr_items ~f:pr_bug bugs

(*******************************************************************
 ** constructors
 *******************************************************************)

(*-------------------------------------------
 * Bug annotation
 *------------------------------------------*)

(*** memory bugs ***)

let mk_bug_type_memory_leak () : bug_type = MemoryLeak None
let mk_bug_type_null_pointer_deref () : bug_type = NullPointerDeref None
let mk_bug_type_buffer_overflow_deref () : bug_type = NullPointerDeref None

(*** integer bugs ***)

let mk_bug_type_integer_overflow () : bug_type = IntegerOverflow None
let mk_bug_type_integer_underflow () : bug_type = IntegerUnderflow None
let mk_bug_type_coercion_error () : bug_type = IntegerCoercionError None
let mk_bug_type_truncation_error () : bug_type = NumericTruncationError None
let mk_bug_type_division_by_zero () : bug_type = DivisionByZero None

(*-------------------------------------------
 * Potential bugs
 *------------------------------------------*)

let mk_potential_bug (instr : instr) (btype : bug_type) : potential_bug =
  { pbug_instr = instr; pbug_func = func_of_instr instr; pbug_type = btype }
;;

(*-------------------------------------------
 * Potential integer bugs
 *------------------------------------------*)

let mk_potential_integer_overflow (instr : instr) : potential_bug =
  let expr = llvalue_of_instr instr in
  let iof =
    { iof_expr = expr;
      iof_bitwidth = LL.integer_bitwidth (LL.type_of expr);
      iof_instr = instr
    } in
  mk_potential_bug instr (IntegerOverflow (Some iof))
;;

let mk_potential_integer_underflow (instr : instr) : potential_bug =
  let expr = llvalue_of_instr instr in
  let iuf =
    { iuf_expr = expr;
      iuf_bitwidth = LL.integer_bitwidth (LL.type_of expr);
      iuf_instr = instr
    } in
  mk_potential_bug instr (IntegerUnderflow (Some iuf))
;;

(*-------------------------------------------
 * Potential memory bugs
 *------------------------------------------*)

let mk_potential_buffer_overflow (instr : instr) : potential_bug =
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
              herror "mk_potential_buffer_overflow: array index not available:"
                pr_instr instr in
          NumElem (size, elem_typ), array_idx
        (* pointer to a dynamically allocated memory *)
        | _ -> MemSizeOf ptr, List.hd_exn idxs in
      { bof_instr = instr;
        bof_pointer = ptr;
        bof_buff_size = size;
        bof_elem_index = index;
        bof_write_operation = false;   (* FIXME: Khanh, please help to compute *)
        bof_stack_based = false;       (* FIXME: Khanh, please help to compute *)
      }
    | _ -> error "mk_buffer_overflow: expect GetElementPtr" in
  mk_potential_bug instr (BufferOverflow (Some bof))
;;

let mk_potential_memory_leak (instr : instr) : potential_bug =
  let mlk = { mlk_pointer = llvalue_of_instr instr; mlk_size = None } in
  mk_potential_bug instr (MemoryLeak (Some mlk))
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
  print ~marker:false ~ruler:`Medium ("Bug Summary:\n" ^ summary)
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

let annotate_potential_bugs (prog : program) : potential_bugs =
  let visit_instr acc instr =
    match instr_opcode instr with
    | LO.Add | LO.Sub | LO.Mul ->
      acc
      @ [ mk_potential_integer_overflow instr ]
      @ [ mk_potential_integer_underflow instr ]
    | LO.SDiv | LO.UDiv ->
      (* TODO: annotate other kind of integer bugs like DivByZero *)
      acc
      @ [ mk_potential_integer_overflow instr ]
      @ [ mk_potential_integer_underflow instr ]
    | LO.GetElementPtr -> acc @ [ mk_potential_buffer_overflow instr ]
    | LO.Ret -> acc @ [ mk_potential_memory_leak instr ]
    | _ -> acc in
  let funcs = prog.prog_user_funcs in
  List.fold_left
    ~f:(fun acc func ->
      acc @ deep_fold_func ~finstr:(Some visit_instr) [] func)
    ~init:[] funcs
;;
