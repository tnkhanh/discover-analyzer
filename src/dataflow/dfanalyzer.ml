(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Dfdata
open Dfbug
open Dfassertion
module LI = Llir
module BG = Bug
module PA = Pointer.Analysis
module MS = Memsize.Analysis
module RG = Range.Analysis
module UA = Undef.Analysis

(*******************************************************************
 ** Supporting functions
 *******************************************************************)

let is_analysis_enabled (analysis : dfa_analysis) : bool =
  List.mem !dfa_analyses analysis ~equal:( == )
  || List.mem !dfa_analyses DfaAllAnalyses ~equal:( == )
;;

(*******************************************************************
 ** Perform pre-analysis passes
 *******************************************************************)

let mark_potential_bugs (pdata : program_data) : program_data =
  let _ = ddebug "Annotating Potential Bug..." in
  let prog = pdata.pdata_program in
  let pbugs = BG.mark_potential_bugs prog in
  let _ = hddebug "POTENTIAL BUGS:" BG.pr_potential_bugs pbugs in
  { pdata with pdata_potential_bugs = pbugs }
;;

let perform_pre_analysis_passes (pdata : program_data) : program_data =
  mark_potential_bugs pdata
;;

(*******************************************************************
 ** Perform main analysis passes
 *******************************************************************)

let perform_range_analysis (pdata : program_data) : program_data =
  if is_analysis_enabled DfaRange
  then (
    let _ = debug ~header:true "Performing Range Analysis..." in
    let prog = pdata.pdata_program in
    let penv = RG.analyze_program prog in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then hprint ~header:true "RANGE INFO" RG.pr_prog_env penv in
    { pdata with pdata_env_range = Some penv })
  else pdata
;;

let perform_undef_analysis (pdata : program_data) : program_data =
  if is_analysis_enabled DfaUndef
  then (
    let _ = debug ~header:true "Performing Undef Analysis" in
    let prog = pdata.pdata_program in
    let penv, time = Sys.track_runtime (fun () -> UA.analyze_program prog) in
    let _ = record_task_time "Undef analysis" time in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then hprint ~header:true "UNDEF INFO" UA.pr_prog_env penv in
    { pdata with pdata_env_undef = Some penv })
  else pdata
;;

let perform_memsize_analysis (pdata : program_data) : program_data =
  if is_analysis_enabled DfaMemsize
  then (
    let _ = debug ~header:true "Performing Memsize Analysis" in
    let prog = pdata.pdata_program in
    let penv = MS.analyze_program prog in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then hprint ~header:true "MEMSIZE INFO" MS.pr_prog_env penv in
    { pdata with pdata_env_memsize = Some penv })
  else pdata
;;

let perform_pointer_analysis (pdata : program_data) : program_data =
  if is_analysis_enabled DfaPointer
  then (
    let _ = debug ~header:true "Performing Pointer Analysis" in
    let prog = pdata.pdata_program in
    let penv, time = Sys.track_runtime (fun () -> PA.analyze_program prog) in
    let _ = record_task_time "Pointer analysis" time in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then hprint ~header:true "POINTER INFO" PA.pr_prog_env penv in
    { pdata with pdata_env_pointer = Some penv })
  else pdata
;;

let reorder_analysis_passes () =
  let _ = warning "TO IMPLEMENET: reorder_analysis passes" in
  ()
;;

let perform_main_analysis_passes (pdata : program_data) : program_data =
  let _ = reorder_analysis_passes () in
  pdata |> perform_undef_analysis |> perform_pointer_analysis
  |> perform_memsize_analysis |> perform_range_analysis
;;

(*******************************************************************
 ** Check assertions
 *******************************************************************)

(* let check_assertions (pdata : program_data) : unit = *)
(*   Option.iter ~f:PA.check_assertions pdata.pdata_env_pointer; *)
(*   Option.iter ~f:RG.check_assertions pdata.pdata_env_range *)
(* ;; *)

let report_analysis_stats (pdata : program_data) : unit =
  Option.iter ~f:PA.report_analysis_stats pdata.pdata_env_pointer
;;

(*******************************************************************
 ** Analysis functions
 *******************************************************************)

let analyze_program (prog : LI.program) : unit =
  let _ = hprint ~ruler:`Long "Analyze program by " pr_dfa_mode !dfa_mode in
  let pdata =
    prog |> mk_program_data |> perform_pre_analysis_passes
    |> perform_main_analysis_passes in
  let _ = report_analysis_stats pdata in
  let _ = check_assertions pdata in
  find_bugs pdata
;;
