(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Libdiscover
open Sprinter
module LL = Llvm
module LX = Lexing

(*******************************************************************
 ** data structures
 *******************************************************************)

type work_mode =
  | WkmSymExec
  | WkmDFA
  | WkmAbsInt
  | WkmNoAnalysis

type dfa_mode =
  | DfaIntraProc
  | DfaInterProc

type precision =
  | Must
  | May

type dfa_analysis =
  | DfaRange
  | DfaUndef
  | DfaMemsize
  | DfaPointer
  | DfaAutoSchedule
  | DfaAllAnalyses

type input_mode =
  | InpUnkn
  | InpSepLogic
  | InpBitcode
  | InpLlir
  | InpCCpp
  | InpGolang

(*******************************************************************
 ** Global Flags
 *******************************************************************)

(*-----------
 * printing
 *----------*)

let print_input_prog = ref false
let print_typed_prog = ref false
let print_core_prog = ref false
let print_analyzed_prog = ref true
let print_type = ref false
let print_concise_output = ref false
let print_concise_debug = ref false
let print_stats_prog = ref false

(* reporting *)
let location_source_code_only = ref false

(* work mode *)
let work_mode = ref WkmNoAnalysis
let skip_analysis = ref false

(* build and release mode *)
let release_mode = ref false (* "false" means debug mode *)

(* analysis properties *)
let dfa_mode = ref DfaInterProc

(* types of analysis *)
let dfa_analyses : dfa_analysis list ref = ref []

(* function to be analyzed *)
let dfa_func_name : string option ref = ref None

(* data exporting *)
let export_entailment = ref false
let export_bitcode = ref false
let export_proof_ascii = ref false
let export_core_prog = ref false
let export_debug_info = ref false
let export_cfg_prog = ref false

(* pointer analysis *)
let dfa_pointer_conservative = ref false

(* sparse analysis *)
let dfa_sparse_analysis = ref true

(* sensitivity *)
let dfa_path_sensitive = ref true
let dfa_context_split_phi = ref false
let dfa_used_globals_selective = ref true

(* let dfa_used_globals_in_func_ptrs = ref true *)
let dfa_used_globals_in_func_ptrs = ref true

(*-------------------------
 * Settings for llvm mode
 *------------------------*)

let llvm_orig_source_name = ref false
let llvm_print_prog_info = ref false
let llvm_simplify = ref true
let llvm_optimize = ref true
let llvm_normalize = ref true

(*-----------------------------
 * Settings for bug detection
 *----------------------------*)

(* General settings *)
let bug_all = ref true

(* integer bugs *)
let bug_integer_all = ref false
let bug_integer_overflow = ref false
let bug_integer_underflow = ref false
let bug_divizion_by_zero = ref false

(* memory bugs *)
let bug_memory_all = ref false
let bug_memory_leak = ref false
let bug_null_pointer_deref = ref false
let bug_buffer_overflow = ref false
let bug_memory_leak = ref false

(*******************************************************************
 ** global variables
 *******************************************************************)

(*------------------
 * File extensions
 *-----------------*)

let file_ext_bitcode = [ "bc" ]
let file_ext_llir = [ "ll" ]
let file_ext_seplogic = [ "sl" ]
let file_ext_c_cpp = [ "c"; "cpp"; "h"; "hpp"; "cc" ]
let file_ext_go = [ "go" ]

(*--------------
 * Input files
 *-------------*)

let input_mode = ref InpUnkn
let input_file = ref ""
let lib_path = "lib"
let lib_core_file = ref (lib_path ^ "/libcore.sc")

(*----------------
 * Configuration
 *---------------*)

let user_config_file = "discover.yaml"
let llvm_version = "13" (* using LLVM 13 *)

let llvm_path = ref ""
let clang_path = ref "clang"
let opt_path = ref "opt"
let gollvm_path = ref ""
let llvm_normalizer_path = ref "normalizer"
let discover_path = Filename.realpath Sys.argv.(0)
let project_path = Filename.dirname discover_path

(*----------------------
 * Compilation options
 *---------------------*)

let clang_user_options = ref ""
let opt_options = ref ""

(*------------
 * Keywords
 *-----------*)

let __result = "result"
let __assert = "__assert_"
let __refute = "__refute_"
let __init = "__init_"
let __assert_no_alias = __assert ^ "no_alias"
let __assert_may_alias = __assert ^ "may_alias"
let __assert_must_alias = __assert ^ "must_alias"
let __refute_no_alias = __refute ^ "no_alias"
let __refute_may_alias = __refute ^ "may_alias"
let __refute_must_alias = __refute ^ "must_alias"
let __assert_range_lower_bound = __assert ^ "range_lower_bound"
let __assert_range_upper_bound = __assert ^ "range_upper_bound"
let __assert_range_full = __assert ^ "range_full"
let __init_globals = __init ^ "globals"

(*------------------
 * Time statistics
 *-----------------*)

let detailed_task_time : (string * float) list ref = ref [] (* tasks and time *)

let sparse_time : float ref = ref 0.0
let analysis_time : float ref = ref 0.0
let total_time : float ref = ref 0.0

(*----------------------
 * Bugs and assertions
 *---------------------*)

let num_of_bugs = ref 0
let num_valid_asserts = ref 0
let num_invalid_asserts = ref 0

(*******************************************************************
 ** location
 *******************************************************************)

type position =
  { pos_file_name : string;
    pos_line_start : int;
    pos_line_end : int;
    pos_col_start : int;
    pos_col_end : int
  }

let mk_position fname lstart lend cstart cend =
  { pos_file_name = fname;
    pos_line_start = lstart;
    pos_line_end = lend;
    pos_col_start = cstart;
    pos_col_end = cend
  }
;;

let mk_position_lexing (pstart : LX.position) (pend : LX.position) : position =
  { pos_file_name = pstart.Lexing.pos_fname;
    pos_line_start = pstart.Lexing.pos_lnum;
    pos_line_end = pend.Lexing.pos_lnum;
    pos_col_start = pstart.Lexing.pos_cnum - pstart.Lexing.pos_bol + 1;
    pos_col_end = pend.Lexing.pos_cnum - pend.Lexing.pos_bol + 1
  }
;;

let sprint_file_excerpt
    filename
    (lstart : int)
    (lend : int)
    (cstart : int)
    (cend : int)
  =
  (* let _ = print_endline ("File: " ^ filename) in *)
  let file_lines = In_channel.read_lines filename in
  let num_lines = List.length file_lines in
  let lstart = if lstart < 3 then 3 else lstart in
  let lend = if lend > num_lines - 2 then num_lines - 2 else lend in
  let rec pr_excerpt lines lcur acc =
    match lines with
    | [] -> List.rev acc
    | line :: nlines ->
      let nl = lcur + 1 in
      if lcur < lstart - 1 || lcur >= lend
      then (
        let marked_line = Printf.sprintf "%6d" nl ^ ".  " ^ line ^ "\n" in
        pr_excerpt nlines (lcur + 1) (marked_line :: acc))
      else (
        let marked_line = Printf.sprintf "%6d" nl ^ ".> " ^ line ^ "\n" in
        let marked_col =
          if lcur = lstart - 1
          then (
            let nc = if cstart > 2 then cstart - 2 else 0 in
            "       > " ^ String.make nc ' ' ^ "^^^\n")
          else if lcur = lend - 1
          then (
            let nc = if cend > 2 then cend - 2 else 0 in
            "       > " ^ String.make nc ' ' ^ "^^^\n")
          else "" in
        pr_excerpt nlines (lcur + 1) (marked_col :: marked_line :: acc)) in
  let excerpt_lines = List.slice file_lines (lstart - 3) (lend + 2) in
  let format_str = pr_excerpt excerpt_lines (lstart - 3) [] in
  String.rstrip (String.concat ~sep:"" format_str)
;;

let sprint_file_position_and_excerpt (p : position) =
  let fname = p.pos_file_name in
  let lstart, lend = p.pos_line_start, p.pos_line_end in
  let cstart, cend = p.pos_col_start, p.pos_col_end in
  let line_column =
    if lstart = lend && cstart = cend
    then sprint_int lstart ^ ":" ^ sprint_int cstart
    else
      sprint_int lstart ^ ":" ^ sprint_int cstart ^ " ~> "
      ^ sprint_int lend ^ ":" ^ sprint_int cend in
  "File: " ^ fname ^ ", line/column position: " ^ line_column ^ "\n"
  ^ sprint_file_excerpt fname lstart lend cstart cend
;;

(*******************************************************************
 ** Data flow analysis
 *******************************************************************)

let equal_precision (p1 : precision) (p2 : precision) : bool = p1 == p2

let merge_precision (p1 : precision) (p2 : precision) : precision =
  if equal_precision p1 p2 then p1 else May
;;

let sprint_dfa_mode (dfa : dfa_mode) : string =
  match dfa with
  | DfaIntraProc -> "Intra-procedural Data-flow Analysis"
  | DfaInterProc -> "Inter-procedural Data-flow Analysis"
;;

let is_pointer_analysis (typ : dfa_analysis) : bool =
  match typ with
  | DfaPointer -> true
  | _ -> false
;;

let name_of_dfa (dfa : dfa_analysis) : string =
  match dfa with
  | DfaRange -> "Range Analysis"
  | DfaUndef -> "Undef Analysis"
  | DfaMemsize -> "Memsize Analysis"
  | DfaPointer -> "Pointer Analysis"
  | DfaAutoSchedule -> "Auto-schedule DFA Analyses"
  | DfaAllAnalyses -> "All Analyses"
;;

(*******************************************************************
 ** utilities function
 *******************************************************************)

let record_task_time (task : string) (time : float) =
  detailed_task_time := !detailed_task_time @ [ task, time ]
;;

let pr_work_mode wm =
  match wm with
  | WkmSymExec -> "Symbolic Execution"
  | WkmDFA -> "Data Flow Analysis"
  | WkmAbsInt -> "Abstract Interpretation"
  | WkmNoAnalysis -> "No Analysis"
;;

(*******************************************************************
 ** Warning and error
 *******************************************************************)

let warning msg =
  let msg = "Warning: " ^ msg in
  if not !print_concise_output then prerr_endline msg
;;

let hwarning msg f x =
  let msg = msg ^ ": " ^ f x in
  warning msg
;;

(** report an error message *)

let error ?(log = "") (msg : string) = raise (EError (msg, log))

(** report 2 error messages *)

let error2 ?(log = "") (msg1 : string) (msg2 : string) =
  let msg = msg1 ^ msg2 in
  error ~log msg
;;

(** report a list of error messages *)

let errors ?(log = "") (msgs : string list) =
  let msg = String.concat ~sep:"" msgs in
  error ~log msg
;;

let herror msg f x =
  let msg = msg ^ f x in
  error msg
;;
