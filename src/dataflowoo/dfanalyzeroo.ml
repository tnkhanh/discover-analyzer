(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LI = Llir
module BG = Bug

module RG = Rangeoo.RangeAnalysis

(*******************************************************************
 ** Data structures
 *******************************************************************)

type program_data = {
  pdata_program : LI.program;
  pdata_potential_bugs : BG.bug list;
  pdata_env_range : RG.prog_env option;
}

(*******************************************************************
 ** Constructor
 *******************************************************************)

let mk_program_data (prog: LI.program) : program_data =
  { pdata_program = prog;
    pdata_potential_bugs = [];
    pdata_env_range = None; }

(*******************************************************************
 ** Supporting functions
 *******************************************************************)

let is_analysis_enabled (analysis: dfa_analysis) : bool =
  (List.mem !dfa_analyses analysis ~equal:(==)) ||
  (List.mem !dfa_analyses DfaAllAnalyses ~equal:(==))

(*******************************************************************
 ** Perform pre-analysis passes
 *******************************************************************)

let annotate_potential_bugs (pdata: program_data) : program_data =
  let _ = ddebug "Annotating Potential Bug..." in
  let prog = pdata.pdata_program in
  let bugs = BG.annotate_potential_bugs prog in
  let _ = hddebugc "Potential Bugs:" BG.pr_potential_bugs bugs in
  {pdata with pdata_potential_bugs = bugs}

let perform_pre_analysis_passes (pdata: program_data) : program_data =
  annotate_potential_bugs pdata

(*******************************************************************
 ** Perform main analysis passes
 *******************************************************************)

let perform_range_analysis (pdata: program_data) : program_data =
  try
    let _ = if not (is_analysis_enabled DfaRange) then raise ESkip in
    let _ = debug ~ruler:`Header "Performing Range Analysis..." in
    let prog = pdata.pdata_program in
    let penv = RG.analyze_program prog in
    let _ = if not !print_concise_output && !print_analyzed_prog then
        hprint ~ruler:`Header "RANGE INFO" RG.pr_prog_env penv in
    {pdata with pdata_env_range = Some penv}
  with ESkip -> pdata

let perform_main_analysis_passes (pdata: program_data) : program_data =
  pdata |>
  perform_range_analysis

(*******************************************************************
 ** Find bugs
 *******************************************************************)

(*-------------------------------------------
 * Integer bugs
 *------------------------------------------*)

let find_bug_integer_overflow (pdata: program_data) =
  let pbugs = List.filter ~f:BG.is_bug_integer_overflow
                pdata.pdata_potential_bugs in
  let bugs = match pdata.pdata_env_range with
    | None -> []
    | Some env ->
      (* TODO: may need to run analysis passes to update bug infor *)
      List.filter ~f:(fun bug -> RG.check_bug env bug == True) pbugs in
  List.map ~f:(BG.mk_real_bug "RangeAnalysis") bugs

let find_bug_integer_underflow (pdata: program_data) =
  let pbugs = List.filter ~f:BG.is_bug_integer_underflow
                pdata.pdata_potential_bugs in
  let bugs = match pdata.pdata_env_range with
    | None -> []
    | Some env ->
      List.filter ~f:(fun bug -> RG.check_bug env bug == True) pbugs in
  List.map ~f:(BG.mk_real_bug "RangeAnalysis") bugs

(* TODO: need a mechanism to schedule analyses based on bugs:
   1. Indetify the type of bugs will be checked
   2. Determine which analyeses need to be performed. *)

let find_all_bugs (pdata: program_data) : unit =
  let _ = println "Checking Bugs..." in
  let prog = pdata.pdata_program in
  let bugs = (find_bug_integer_overflow pdata) @
             (find_bug_integer_underflow pdata) in
  let _ = List.iter ~f:BG.report_bug bugs in
  let _ = num_of_bugs := List.length bugs in
  BG.report_bug_stats bugs

(*******************************************************************
 ** Check assertions
 *******************************************************************)

let check_assertions (pdata: program_data) : unit =
  match pdata.pdata_env_range with
  | None -> ()
  | Some env -> RG.check_assertions env

let report_analysis_stats (pdata: program_data) : unit =
  ()

(*******************************************************************
 ** Analysis functions
 *******************************************************************)

let analyze_program_llvm (prog: LI.program) : unit =
  let _ = hprint ~ruler:`Long "Analyze program by " pr_dfa_mode !dfa_mode in
  let pdata = prog |> mk_program_data |>
              perform_pre_analysis_passes |>
              perform_main_analysis_passes in
  let _ = report_analysis_stats pdata in
  let _ = check_assertions pdata in
  find_all_bugs pdata
