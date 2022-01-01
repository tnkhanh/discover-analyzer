(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(* This module contains declaration of global variables *)

open Core
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
  | DfaMemSize
  | DfaMemType
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
  | InpSolidity

(*******************************************************************
 ** Global Flags
 *******************************************************************)

(*-----------
 * printing
 *----------*)

let print_input_prog = ref false
let print_instrumented_prog = ref false
let print_typed_prog = ref false
let print_core_prog = ref false
let print_analyzed_prog = ref true
let print_type = ref false
let print_concise_output = ref false
let print_concise_debug = ref false
let print_stats_prog = ref false

(* reporting *)
let location_source_code_only = ref false

(* bug annotation *)
let bug_annotation = ref false

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

(*----------------------------------
 * Settings for assertion checking
 *---------------------------------*)

let check_assert = ref false
let assert_all = ref false
let assert_range = ref false
let assert_pointer = ref false


(*-----------------------------
 * Settings for bug detection
 *----------------------------*)

(* General settings *)
let find_bug = ref false
let bug_all = ref false

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
let file_ext_solidity = [ "sol" ]

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
let clang_exe = ref "clang"
let opt_exe = ref "opt"
let llvm_dis_exe = ref "llvm-dis"
let normalizer_exe = ref "normalizer"
let solang_exe = ref "solang"
let gollvm_path = ref ""
let discover_path = Filename.realpath Sys.argv.(0)
let project_path = Filename.dirname discover_path

(*----------------------
 * Compilation options
 *---------------------*)

let clang_user_options = ref ""
let solang_user_options = ref ""
let opt_user_options = ref ""

(*------------
 * Keywords
 *-----------*)

let __result = "result"
let __assert = "__assert_"
let __refute = "__refute_"
let __assume = "__assume_"
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
let __assume_range = __assume ^ "range"
let __init_globals = __init ^ "globals"

(*------------
 * Reporting key words
 *-----------*)
let __report_valid_assert = "- Valid assertions: "
let __report_invalid_assert = "- Invalid assertions: "
(*let _ok_status = "OK!"*)
(*let _failed_status = "FAILED!"*)

(*------------------
 * Time statistics
 *-----------------*)

let detailed_task_time : (string * float) list ref = ref []
(* tasks and time *)

let sparse_time : float ref = ref 0.0
let analysis_time : float ref = ref 0.0
let total_time : float ref = ref 0.0

(*----------------------
 * Bugs and assertions
 *---------------------*)

let num_detected_bugs = ref 0
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

(*******************************************************************
 ** Data flow analysis
 *******************************************************************)

let equal_precision (p1 : precision) (p2 : precision) : bool = p1 == p2

let merge_precision (p1 : precision) (p2 : precision) : precision =
  if equal_precision p1 p2 then p1 else May
;;

let pr_dfa_mode (dfa : dfa_mode) : string =
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
  | DfaMemSize -> "MemSize Analysis"
  | DfaMemType -> "MemType Analysis"
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
