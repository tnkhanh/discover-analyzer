(******************************************************************
 ** Author: Ta Quang Trung
 ** Date: 2020
 ******************************************************************)

open Core
open Lib
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

(* printing *)
let print_input_prog = ref false
let print_typed_prog = ref false
let print_core_prog = ref false
let print_analyzed_prog = ref true
let print_type = ref false
let print_concise_output = ref false
let print_concise_debug = ref false

let print_stats_prog = ref false

(* work mode *)
let work_mode = ref WkmNoAnalysis
let skip_analysis = ref false

(* build and release mode *)
let release_mode = ref false              (* "false" means debug mode *)

(***************************************)
(********* Data-flow analysis **********)

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

(* reporting *)
let location_source = ref false
let location_bitcode = ref true

(* llvm mode *)
let llvm_orig_source_name = ref false
let llvm_print_prog_info = ref false
let llvm_simplify = ref true
let llvm_optimize = ref true
let llvm_normalize = ref true

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

(*******************************************************************
 ** global variables
 *******************************************************************)

(* file extensions *)
let file_ext_bitcode = ["bc"]
let file_ext_llir = ["ll"]
let file_ext_seplogic = ["sl"]
let file_ext_c_cpp = ["c"; "cpp"; "h"; "hpp"; "cc"]
let file_ext_go = ["go"]

(* input files *)
let input_mode = ref InpUnkn
let input_file = ref ""
let lib_path = "lib"
let lib_core_file = ref (lib_path ^ "/libcore.sc")

(* user_configuration *)
let user_config_file = "discover.yaml"

(* llvm and clang version *)
let llvm_version = "11"              (* using LLVM 11 *)
let llvm_path = ref ""
let clang_path = ref "clang"
let opt_path = ref "opt"

(* gollvm path *)
let gollvm_path = ref ""

(* normalizer *)
let llvm_normalizer_path = ref "normalizer"

(* compilation options *)
let clang_options = ref ""
let clang_extra_options = ref ""
let opt_options = ref ""

(* keywords *)
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

(* time statistics *)
let detailed_task_time : (string * float) list ref = ref []  (* tasks and time *)
let sparse_time : float ref = ref 0.0
let analysis_time : float ref = ref 0.0
let total_time : float ref = ref 0.0

(* bugs and assertions *)
let num_of_bugs = ref 0
let num_valid_asserts = ref 0
let num_invalid_asserts = ref 0

(*******************************************************************
 ** location
 *******************************************************************)

type location =
  | LRange of { lrg_filename : string;
                lrg_linum_begin : int;
                lrg_linum_end : int;
                lrg_colnum_begin : int;
                lrg_colnum_end : int; }
  | LPoint of { lpt_filename : string;
                lpt_linum : int;
                lpt_colnum : int; }

let mk_location_range (bpos: Lexing.position) (epos: Lexing.position) : location =
  let lnum_begin = bpos.Lexing.pos_lnum in
  let cnum_begin = bpos.Lexing.pos_cnum - bpos.Lexing.pos_bol + 1 in
  let lnum_end = epos.Lexing.pos_lnum in
  let cnum_end = epos.Lexing.pos_cnum - epos.Lexing.pos_bol + 1 in
  LRange { lrg_filename = bpos.Lexing.pos_fname;
           lrg_linum_begin = lnum_begin;
           lrg_linum_end = lnum_end;
           lrg_colnum_begin = cnum_begin;
           lrg_colnum_end = cnum_end; }

let mk_location_point filename linum colnum : location =
  LPoint { lpt_filename = filename;
           lpt_linum = linum;
           lpt_colnum = colnum;}

let dummy_loc = mk_location_range Lexing.dummy_pos Lexing.dummy_pos

let pr_file filename start stop =
  let _ = print_endline ("File: " ^ filename) in
  let lines = In_channel.read_lines filename in
  let start_pr = if start < 3 then 3 else start in
  let stop_pr =
    if stop > (List.length lines) - 2 then (List.length lines) - 2
    else stop in
  let rec format_code line acc = function
    | [] -> List.rev acc
    | h :: t ->
      if (line < start-1 || line >= stop) then
        format_code (line+1)
          (("   " ^ string_of_int (line+1) ^ ".  " ^ h ^ "\n")::acc) t
      else
        format_code (line+1)
          (("   " ^ string_of_int (line+1) ^ ".> " ^ h ^ "\n")::acc) t in
  let lines_str = List.slice lines (start_pr-3) (stop_pr+2) in
  let format_str = format_code (start_pr-3) [] lines_str in
  String.concat ~sep:"" (format_str)

let pr_location loc =
  match loc with
  | LRange l ->
    "file: " ^ l.lrg_filename ^ ", " ^
    (pr_int l.lrg_linum_begin) ^ ":" ^ (pr_int l.lrg_colnum_begin) ^ " ~> " ^
    (pr_int l.lrg_linum_end) ^ ":" ^ (pr_int l.lrg_colnum_end) ^
    "\n\n" ^ (pr_file l.lrg_filename l.lrg_linum_begin l.lrg_linum_end)
  | LPoint l ->
    "file: " ^ l.lpt_filename ^ ", " ^
    "line: " ^ (pr_int l.lpt_linum) ^ ", " ^
    "col: " ^ (pr_int l.lpt_colnum) ^
    "\n\n" ^ (pr_file l.lpt_filename l.lpt_linum l.lpt_linum)

let get_file_name_of_location (loc: location) : string =
  match loc with
  | LRange l -> l.lrg_filename
  | LPoint l -> l.lpt_filename

let get_line_numbers_of_location (loc: location) : (int * int) =
  match loc with
  | LRange l -> (l.lrg_linum_begin, l.lrg_linum_end)
  | LPoint l -> (l.lpt_linum, l.lpt_linum)


(*******************************************************************
 ** Data flow analysis
 *******************************************************************)

let equal_precision (p1: precision) (p2: precision) : bool =
  p1 == p2

let merge_precision (p1: precision) (p2: precision) : precision =
  if equal_precision p1 p2 then p1
  else May

let pr_dfa_mode (dfa: dfa_mode) : string =
  match dfa with
  | DfaIntraProc -> "Intra-procedural Data-flow Analysis"
  | DfaInterProc -> "Inter-procedural Data-flow Analysis"

let is_pointer_analysis (typ: dfa_analysis) : bool =
  match typ with
  | DfaPointer -> true
  | _ -> false

let name_of_dfa (dfa: dfa_analysis) : string =
  match dfa with
  | DfaRange -> "Range Analysis"
  | DfaUndef -> "Undef Analysis"
  | DfaMemsize -> "Memsize Analysis"
  | DfaPointer -> "Pointer Analysis"
  | DfaAllAnalyses -> "All Analyses"


(*******************************************************************
 ** utilities function
 *******************************************************************)

let record_task_time (task: string) (time: float) =
  detailed_task_time := !detailed_task_time @ [(task, time)]

let pr_work_mode wm =
  match wm with
  | WkmSymExec -> "Symbolic Execution"
  | WkmDFA -> "Data Flow Analysis"
  | WkmAbsInt -> "Abstract Interpretation"
  | WkmNoAnalysis -> "No Analysis"

(*******************************************************************
 ** Warning and error
 *******************************************************************)

let warning msg =
  let msg = "Warning: " ^ msg in
  if not !print_concise_output then
    prerr_endline msg

let hwarning msg f x =
  let msg = msg ^ ": " ^ (f x) in
  warning msg

let error ?(log="") msg =
  raise (EError (msg, log))

let errors ?(log="") (msgs: string list) =
  let msg = String.concat ~sep:"" msgs in
  error ~log msg

let herror msg f x =
  let msg = msg ^ (f x) in
  error msg
