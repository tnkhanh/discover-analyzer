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

type dfa_result =
  { dfa_total_analysis_time : float;
    dfa_num_detected_bugs : int;
    dfa_num_valid_asserts : int;
    dfa_num_invalid_asserts : int
  }

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

let mark_potential_bugs (dfa : dfa_data) : dfa_data =
  let _ = ddebug "Annotating Potential Bug..." in
  let prog = dfa.dfa_program in
  let pbugs = BG.mark_potential_bugs prog in
  let _ = ddebugp "POTENTIAL BUGS:" BG.pr_potential_bugs pbugs in
  { dfa with dfa_potential_bugs = pbugs }
;;

let perform_pre_analysis_passes (dfa : dfa_data) : dfa_data =
  mark_potential_bugs dfa
;;

(*******************************************************************
 ** Perform main analysis passes
 *******************************************************************)

let perform_range_analysis (dfa : dfa_data) : dfa_data =
  if is_analysis_enabled DfaRange
  then (
    let _ = debug ~header:true "Performing Range Analysis..." in
    let prog = dfa.dfa_program in
    let penv = RG.analyze_program prog in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then printp ~header:true "Range Info" RG.pr_prog_env penv in
    { dfa with dfa_env_range = Some penv })
  else dfa
;;

let perform_undef_analysis (dfa : dfa_data) : dfa_data =
  if is_analysis_enabled DfaUndef
  then (
    let _ = debug ~header:true "Performing Undef Analysis" in
    let prog = dfa.dfa_program in
    let penv, time = Sys.track_runtime ~f:(fun () -> UA.analyze_program prog) in
    let _ = record_task_time "Undef analysis" time in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then printp ~header:true "Undef Info" UA.pr_prog_env penv in
    { dfa with dfa_env_undef = Some penv })
  else dfa
;;

let perform_memsize_analysis (dfa : dfa_data) : dfa_data =
  if is_analysis_enabled DfaMemSize
  then (
    let _ = debug ~header:true "Performing MemSize Analysis" in
    let prog = dfa.dfa_program in
    let penv = MS.analyze_program prog in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then printp ~header:true "MemSize Info" MS.pr_prog_env penv in
    { dfa with dfa_env_memsize = Some penv })
  else dfa
;;

let perform_memtype_analysis (dfa : dfa_data) : dfa_data =
  if is_analysis_enabled DfaMemType
  then (
    let _ = debug ~header:true "Performing MemType Analysis" in
    let prog = dfa.dfa_program in
    let penv = MT.analyze_program prog in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then printp ~header:true "MemType Info" MT.pr_prog_env penv in
    { dfa with dfa_env_memtype = Some penv })
  else dfa
;;

let perform_pointer_analysis (dfa : dfa_data) : dfa_data =
  if is_analysis_enabled DfaPointer
  then (
    let _ = debug ~header:true "Performing Pointer Analysis" in
    let prog = dfa.dfa_program in
    let penv, time = Sys.track_runtime ~f:(fun () -> PA.analyze_program prog) in
    let _ = record_task_time "Pointer analysis" time in
    let _ =
      if (not !print_concise_output) && !print_analyzed_prog
      then printp ~header:true "Pointer Info" PA.pr_prog_env penv in
    { dfa with dfa_env_pointer = Some penv })
  else dfa
;;

let reorder_analysis_passes () =
  let _ = warning "TO IMPLEMENET: reorder_analysis passes" in
  ()
;;

let perform_main_analysis_passes (dfa : dfa_data) : dfa_data =
  let _ = reorder_analysis_passes () in
  dfa |> perform_undef_analysis |> perform_pointer_analysis
  |> perform_memsize_analysis |> perform_memtype_analysis
  |> perform_range_analysis
;;

(*******************************************************************
 ** Check assertions
 *******************************************************************)

(* let check_assertions (dfa : dfa_data) : unit = *)
(*   Option.iter ~f:PA.check_assertions dfa.dfa_env_pointer; *)
(*   Option.iter ~f:RG.check_assertions dfa.dfa_env_range *)
(* ;; *)

let report_analysis_stats (dfa : dfa_data) : unit =
  Option.iter ~f:PA.report_analysis_stats dfa.dfa_env_pointer
;;

(*******************************************************************
 ** Analysis functions
 *******************************************************************)

let compute_total_analysis_time (dfa : dfa_data) : float =
  let open Option.Let_syntax in
  let analysis_runtimes =
    [ (dfa.dfa_env_undef >>= fun p -> return p.penv_analysis_time) ]
    @ [ (dfa.dfa_env_pointer >>= fun p -> return p.penv_analysis_time) ] in
  List.fold
    ~f:(fun acc t -> Option.value t ~default:0. +. acc)
    ~init:0. analysis_runtimes
;;

let compute_analysis_result (dfa : dfa_data) (bugs : BG.bug list) : dfa_result =
  let num_detected_bugs = ref 0 in
  let num_valid_asserts = ref 0 in
  let num_invalid_asserts = ref 0 in
  { dfa_total_analysis_time = compute_total_analysis_time dfa;
    dfa_num_detected_bugs = !num_detected_bugs;
    dfa_num_valid_asserts = !num_valid_asserts;
    dfa_num_invalid_asserts = !num_invalid_asserts
  }
;;

let analyze_program (prog : LI.program) : dfa_result =
  let _ = printp ~ruler:`Long "Analyze program by " pr_dfa_mode !dfa_mode in
  let dfa =
    prog |> mk_dfa_data |> perform_pre_analysis_passes
    |> perform_main_analysis_passes in
  let _ = report_analysis_stats dfa in
  let _ = check_assertions dfa in
  let bugs = find_bugs dfa in
  compute_analysis_result dfa bugs
;;
