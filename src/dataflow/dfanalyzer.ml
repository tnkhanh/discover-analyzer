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

module PA = Pointer.PointerAnalysis
module MS = Memsize.MemsizeAnalysis
module RG = Range.RangeAnalysis
module UA = Undef.UndefAnalysis


(*******************************************************************
 ** Data structures
 *******************************************************************)


type program_data = {
  pdata_program : LI.program;
  pdata_potential_bugs : BG.bug list;
  pdata_env_memsize : MS.prog_env list;
  pdata_env_pointer : PA.prog_env list;
  pdata_env_range : RG.prog_env list;
  pdata_env_undef : UA.prog_env list;
}


(*******************************************************************
 ** Constructor
 *******************************************************************)

let mk_program_data (prog: LI.program) : program_data =
  { pdata_program = prog;
    pdata_potential_bugs = [];
    pdata_env_memsize = [];
    pdata_env_pointer = [];
    pdata_env_range = [];
    pdata_env_undef = []; }


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
    {pdata with pdata_env_range = [penv]}
  with ESkip -> pdata


let perform_undef_analysis (pdata: program_data) : program_data =
  try
    let _ = if not (is_analysis_enabled DfaUndef) then raise ESkip in
    let _ = debug ~ruler:`Header "Performing Undef Analysis" in
    let prog = pdata.pdata_program in
    let penv, time = track_runtime (fun () -> UA.analyze_program prog) in
    let _ = record_task_time "Undef analysis" time in
    let _ = if not !print_concise_output && !print_analyzed_prog then
        hprint ~ruler:`Header "UNDEF INFO" UA.pr_prog_env penv in
    {pdata with pdata_env_undef = [penv]}
  with ESkip -> pdata


let perform_memsize_analysis (pdata: program_data) : program_data =
  try
    let _ = if not (is_analysis_enabled DfaMemsize) then raise ESkip in
    let _ = debug ~ruler:`Header "Performing Memsize Analysis" in
    let prog = pdata.pdata_program in
    let penv = MS.analyze_program prog in
    let _ = if not !print_concise_output && !print_analyzed_prog then
        hprint ~ruler:`Header "MEMSIZE INFO" MS.pr_prog_env penv in
    {pdata with pdata_env_memsize = [penv]}
  with ESkip -> pdata


let perform_pointer_analysis (pdata: program_data) : program_data =
  try
    let _ = if not (is_analysis_enabled DfaPointer) then raise ESkip in
    let _ = debug ~ruler:`Header "Performing Pointer Analysis" in
    let prog = pdata.pdata_program in
    let penv, time = track_runtime (fun () -> PA.analyze_program prog) in
    let _ = record_task_time "Pointer analysis" time in
    let _ = if not !print_concise_output && !print_analyzed_prog then
        hprint ~ruler:`Header "POINTER INFO" PA.pr_prog_env penv in
    {pdata with pdata_env_pointer = [penv]}
  with ESkip -> pdata


let perform_main_analysis_passes (pdata: program_data) : program_data =
  pdata |>
  perform_undef_analysis |>
  perform_pointer_analysis |>
  perform_memsize_analysis |>
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
  let bugs = List.filter
               ~f:(fun bug ->
                    List.exists ~f:(fun env -> RG.check_bug env bug == True)
                      pdata.pdata_env_range)
               pbugs in
  List.map ~f:(BG.mk_real_bug "RangeAnalysis") bugs


let find_bug_integer_underflow (pdata: program_data) =
  let pbugs = List.filter ~f:BG.is_bug_integer_underflow
                pdata.pdata_potential_bugs in
  let bugs = List.filter
               ~f:(fun bug ->
                    List.exists ~f:(fun env -> RG.check_bug env bug == True)
                      pdata.pdata_env_range)
               pbugs in
  List.map ~f:(BG.mk_real_bug "RangeAnalysis") bugs


(*-------------------------------------------
 * Memory bugs
 *------------------------------------------*)

(* TODO: use 2 analyses for buffer overflow: range anlaysis and memsize *)
let check_buffer_overflow (bof: BG.buffer_overflow) pdata : ternary =
  try
    if not (!bug_memory_all || !bug_buffer_overflow) then
      raise (ETern Unkn);

    List.iter
      ~f:(fun penv ->
           let func = LI.func_of_instr bof.bof_instr in
           let fenvs = match Hashtbl.find penv.penv_func_envs func with
             | None -> []
             | Some fenvs -> fenvs in
           let res =
             List.exists
               ~f:(fun fenv ->
                    match RG.get_instr_output fenv bof.bof_instr with
                    | None -> false
                    | Some data ->
                      let itv = RG.get_interval
                                  (LI.expr_of_llvalue bof.bof_elem_index) data in
                      match bof.bof_buff_size with
                      | None -> false
                      | Some n ->
                        (* TODO: need to run memsize passes to get buffer size *)
                        (* if RG.ID.compare_interval_upper_bound_with_int itv n >= 0 then true *)
                        (* else false *)
                        true)
               fenvs
           in
           if res then raise (ETern  True))
      pdata.pdata_env_range;
    False
  with ETern res -> res


let find_bug_buffer_overflow (pdata: program_data) =
  List.fold_left
    ~f:(fun acc bug ->
         if not (BG.is_bug_buffer_overflow bug) then acc
         else if List.exists ~f:(fun env -> RG.check_bug env bug == True)
                   pdata.pdata_env_range then
           acc @ [BG.mk_real_bug "RangeAnalysis" bug]
         else acc)
    ~init:[] pdata.pdata_potential_bugs


let find_bug_memory_leak (pdata: program_data) =
  List.fold_left
    ~f:(fun acc bug ->
         if not (BG.is_bug_memory_leak bug) then acc
         else if List.exists ~f:(fun env -> MS.check_bug env bug == True)
                   pdata.pdata_env_memsize then
           acc @ [BG.mk_real_bug "MemsizeAnalysis" bug]
         else if List.exists ~f:(fun env -> PA.check_bug env bug == True)
                   pdata.pdata_env_pointer then
           acc @ [BG.mk_real_bug "PointerAnalysis" bug]
         else acc)
    ~init:[] pdata.pdata_potential_bugs


(** TODO: need a mechanism to schedule analyses based on bugs:
    - Indetify the type of bugs will be checked
    - Determine which analyeses need to be performed. *)

let find_all_bugs (pdata: program_data) : unit =
  let _ = println "Checking Bugs..." in
  let prog = pdata.pdata_program in
  let bugs = (find_bug_memory_leak pdata) @
             (find_bug_buffer_overflow pdata) @
             (find_bug_integer_overflow pdata) @
             (find_bug_integer_underflow pdata) in
  let _ = List.iter ~f:BG.report_bug bugs in
  let _ = num_of_bugs := List.length bugs in
  BG.report_bug_stats bugs


(*******************************************************************
 ** Check assertions
 *******************************************************************)

let check_assertions (pdata: program_data) : unit =
  let _ = print_endline "\nChecking assertions..." in
  List.iter ~f:PA.check_assertions pdata.pdata_env_pointer;
  List.iter ~f:RG.check_assertions pdata.pdata_env_range


let report_analysis_stats (pdata: program_data) : unit =
  List.iter ~f:PA.report_analysis_stats pdata.pdata_env_pointer


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
