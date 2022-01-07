(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
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
    dfa_detailed_analysis_time : (string * float) list;
    dfa_num_detected_bugs : int;
    dfa_detailed_detected_bugs : (string * int) list;
    dfa_num_valid_asserts : int;
    dfa_num_invalid_asserts : int
  }

type benchmark_result =
  {
    ben_correct_bug_reports : int;
    ben_incorrect_bug_reports: int;
    ben_missing_bugs : int;
    ben_detailed_result : string;
  }

(*let dummy_ben_result =*)
  (*{*)
    (*ben_found_bugs = 0;*)
    (*ben_missing_bugs = 0;*)
    (*ben_incorrect_bug_reports = 0;*)
    (*ben_detailed_result = "";*)
  (*}*)

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
    let penv = UA.analyze_program prog in
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
    let penv = PA.analyze_program prog in
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

let update_analysis_time (dfa : dfa_data) (res : dfa_result) : dfa_result =
  let open Option.Let_syntax in
  let analysis_runtimes =
    [ (dfa.dfa_env_pointer
      >>= fun p -> return (p.penv_analysis_name, p.penv_analysis_time));
      (dfa.dfa_env_memtype
      >>= fun p -> return (p.penv_analysis_name, p.penv_analysis_time));
      (dfa.dfa_env_memsize
      >>= fun p -> return (p.penv_analysis_name, p.penv_analysis_time));
      (dfa.dfa_env_range
      >>= fun p -> return (p.penv_analysis_name, p.penv_analysis_time));
      (dfa.dfa_env_undef
      >>= fun p -> return (p.penv_analysis_name, p.penv_analysis_time))
    ]
    |> List.filter_opt in
  let total_time =
    List.fold ~f:(fun acc (_, t) -> acc +. t) ~init:0. analysis_runtimes in
  { res with
    dfa_total_analysis_time = total_time;
    dfa_detailed_analysis_time = analysis_runtimes
  }
;;

let compute_bug_detailed_info (bugs : BG.bug list) : (string * int) list =
  let bug_info = Hashtbl.create (module String) in
  let _ =
    List.iter
      ~f:(fun bug ->
        let bug_name = BG.pr_bug_name bug in
        Hashtbl.update
          ~f:(fun count ->
            match count with
            | None -> 1
            | Some n -> n + 1)
          bug_info bug_name)
      bugs in
  Hashtbl.fold ~f:(fun ~key ~data acc -> (key, data) :: acc) ~init:[] bug_info
;;

let compute_analysis_result (dfa : dfa_data) (bugs : BG.bug list) : dfa_result =
  let num_valid_asserts = ref 0 in
  let num_invalid_asserts = ref 0 in
  let res =
    { dfa_total_analysis_time = 0.;
      dfa_detailed_analysis_time = [];
      dfa_num_detected_bugs = List.length bugs;
      dfa_detailed_detected_bugs = compute_bug_detailed_info bugs;
      dfa_num_valid_asserts = !num_valid_asserts;
      dfa_num_invalid_asserts = !num_invalid_asserts
    } in
  update_analysis_time dfa res
;;

let compute_benchmark_result (prog : LI.program) (bugs : BG.bug list) : benchmark_result =
  let finstr =
    Some (fun acc instr ->
      if LI.is_instr_call instr then
        (let func_name = LI.func_name (LI.callee_of_instr_call instr) in
        if String.is_prefix ~prefix:__assert func_name
        then (instr, None) :: acc
        else
          acc)
      else
       acc)
  in
  let assert_calls = LI.visit_fold_program ~finstr [] prog in
  let _ = print "Assert calls: " in
  let _ =
    List.iter ~f:print
      (List.map assert_calls ~f: (fun (c, _) -> LL.string_of_llvalue (LI.llvalue_of_instr c))) in

  let correct_bug_reports, incorrect_bug_reports, matched_calls = 
    List.fold ~init:(0, 0, assert_calls) ~f:(fun (correct, incorrect, mcalls) bug ->

      let found, new_rev_mcalls = 
        List.fold ~init:(false, []) ~f:(fun (f, new_mcalls) ((call, _) as mcall) ->
          if f then
            (true, mcall :: new_mcalls)
          else
            let asserted_ins = LI.operand call 0 in
            let bug_ins = LI.llvalue_of_instr bug.bug_instr in
            (* TODO: nested folds need inspection, and the comparison below *)
            if asserted_ins == bug_ins then
              (
                let callee_name = LI.func_name (LI.callee_of_instr_call call) in
                if String.is_substring 
                     ~substring:(BG.pr_bug_type_lowercase bug.bug_type) callee_name
                then
                  (true, (call, Some bug) :: new_mcalls)
                else
                  false, mcall :: new_mcalls
              )
            else
              false, mcall :: new_mcalls
        ) mcalls in
      ((if found then correct + 1 else correct),
       (if found then incorrect else incorrect + 1),
       List.rev new_rev_mcalls)
    ) bugs in
  let detailed_result, missing_bugs =
    List.fold ~init:("", 0) ~f:(fun (acc, count) (call, bug_opt) ->
      match bug_opt with
      | None -> acc, count + 1
      | Some bug -> 
        (let call_str = LL.string_of_llvalue (LI.llvalue_of_instr call) in 
        acc ^ "Match:\n  Call: " ^ call_str ^ "\n  Bug: " ^ bug.bug_reason ^ "\n"),
        count
    ) matched_calls in
  {
    ben_correct_bug_reports = correct_bug_reports;
    ben_incorrect_bug_reports = incorrect_bug_reports;
    ben_missing_bugs = missing_bugs;
    ben_detailed_result = detailed_result;
  }
;;

let analyze_program (prog : LI.program) : (dfa_result * benchmark_result) =
  let _ = printp ~ruler:`Long "Analyze program by " pr_dfa_mode !dfa_mode in
  let dfa =
    prog |> mk_dfa_data |> perform_pre_analysis_passes
    |> perform_main_analysis_passes in
  let _ = report_analysis_stats dfa in
  let _ = check_assertions dfa in
  let bugs = find_bugs dfa in
  (compute_analysis_result dfa bugs, 
   compute_benchmark_result prog bugs)
;;
