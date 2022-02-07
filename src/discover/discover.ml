(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module AG = Argument
module AS = Assertion
module CI = Commonir
module DA = Dfanalyzer
module SE = Symexec
module CP = Compile
module VS = Version
module BM = Benchmark

type analysis_result =
  | RDfa of (DA.dfa_result * BM.benchmark_result)
  | RSymexec of SE.symexec_result
  | RNone

let init_solvers () = Z3.config_z3_solver ()

let config_discover () : unit =
  let _ = discover_version := !discover_version ^ "-git:" ^ VS.git_revision in
  let _ = discover_version := !discover_version ^ ":" ^ VS.git_time in
  let discover_dir = Filename.dirname Sys.executable_name in
  let _ = lib_core_file := discover_dir ^ "/" ^ !lib_core_file in
  let user_config_file = discover_dir ^ "/" ^ user_config_file in
  match Sys.file_exists user_config_file with
  | `No | `Unknown -> ()
  | `Yes ->
    let _ = print "Read user configuration" in
    let file_content = In_channel.read_all user_config_file in
    (match Yaml.of_string file_content with
    | Result.Error (`Msg msg) ->
      error ("Failed to read the configuration file: " ^ user_config_file)
    | Result.Ok config ->
      let _ =
        match Ezjsonm.find_opt config [ "GOLLVM_BINARY_PATH" ] with
        | Some path ->
          let _ = gollvm_bin_path := Ezjsonm.get_string path in
          if not (String.is_suffix ~suffix:"/" !gollvm_bin_path)
          then gollvm_bin_path := !gollvm_bin_path ^ "/"
        | None -> () in
      let _ =
        match Ezjsonm.find_opt config [ "LLVM_BINARY_PATH" ] with
        | Some path ->
          let _ = llvm_bin_path := Ezjsonm.get_string path in
          if not (String.is_suffix ~suffix:"/" !llvm_bin_path)
          then llvm_bin_path := !llvm_bin_path ^ "/"
        | None -> () in
      ())
;;

let init_environment () =
  let _ = config_discover () in
  let _ = CP.config_toolchain () in
  let _ = init_solvers () in
  if !skip_analysis then work_mode := WkmNoAnalysis
;;

let print_discover_settings () =
  let _ = print ("Running Discover: " ^ !discover_version) in
  let _ = print ~always:true ("Checking file: " ^ !input_file) in
  let info =
    [ "Discover's settings:";
      "- Clang: " ^ !clang_exe ^ " (" ^ !clang_version ^ ")";
      "- Llvm-opt: " ^ !llvm_opt_exe ^ " (" ^ !llvm_opt_version ^ ")";
      "- Llvm-dis: " ^ !llvm_dis_exe ^ " (" ^ !llvm_dis_version ^ ")";
      "- Normalizer: " ^ !normalizer_exe ^ " (" ^ !normalizer_version ^ ")";
      "- Solang: " ^ !solang_exe ^ " (" ^ !solang_version ^ ")";
      "- Gollvm: " ^ !gollvm_exe ^ " (" ^ !gollvm_version ^ ")";
      "- Z3: " ^ !Z3.z3_exe ^ " (" ^ !Z3.z3_version ^ ")"
    ] in
  debug (beautiful_concat ~sep:"\n" info)
;;

let clean_environment () = Smt.stop_solver ()

let _enable_release_mode_alias_analysis () =
  let _ = release_mode := true in
  let _ = work_mode := WkmDFA in
  let _ = dfa_analyses := [ DfaPointer ] in
  let _ = dfa_mode := DfaInterProc in
  let _ = no_debug := true in
  let _ = no_print := true in
  let _ = print_concise_output := true in
  print_stats_prog := true
;;

let create_runtime_summary (res : analysis_result) : string =
  match res with
  | RDfa (rdfa, rben) ->
    let total_time =
      sprintf "- Analysis time: %.2fs\n" rdfa.DA.dfa_total_analysis_time in
    let detailed_runtime =
      if (not !mode_debug) || List.is_empty rdfa.dfa_detailed_analysis_time
      then ""
      else
        List.fold_left
          ~f:(fun acc (analysis, time) ->
            acc ^ "  + " ^ analysis ^ ": " ^ sprintf "%.2fs\n" time)
          ~init:"" rdfa.dfa_detailed_analysis_time in
    total_time ^ detailed_runtime
  | _ -> ""
;;

let create_assertion_summary (res : analysis_result) : string =
  if !check_assert
  then
    sprintf "- Valid assertions: %d\n" !num_valid_asserts
    ^ sprintf "- Invalid assertions: %d\n" !num_invalid_asserts
  else "- Assertion checking is not enabled\n"
;;

let create_bug_summary (res : analysis_result) : string =
  if !find_bug
  then (
    match res with
    | RDfa (rdfa, _) ->
      let total_bugs =
        sprintf "- Detected bugs: %d\n" rdfa.dfa_num_detected_bugs in
      let detailed_bugs =
        List.fold_left
          ~f:(fun acc (bug_name, n) ->
            acc ^ "  + " ^ bug_name ^ ": " ^ pr_int n ^ "\n")
          ~init:"" rdfa.dfa_detailed_detected_bugs in
      total_bugs ^ detailed_bugs
    | _ -> "")
  else "- Bug finding is not enabled\n"
;;

let create_benchmark_summary (res : analysis_result) : string =
  if !print_benchmark then
  match res with
  | RDfa (_, rben) ->
    let short_summary =
      "===\nBenchmark summary:\n" ^ __report_correct_bug
      ^ pr_int rben.ben_correct_bug_reports
      ^ "\n" ^ __report_incorrect_bug
      ^ pr_int rben.ben_incorrect_bug_reports
      ^ "\n" ^ __report_missing_bug
      ^ pr_int rben.ben_missing_bugs
      ^ "\n" in

    short_summary ^ rben.ben_detailed_result ^ "===\n"
  | _ -> ""
  else ""
;;

let print_analysis_summary (res : analysis_result) (total_time : float) : unit =
  match !work_mode with
  | WkmNoAnalysis -> ()
  | _ ->
    let assertion_summary = create_assertion_summary res in
    let runtime_summary = create_runtime_summary res in
    let bug_summary = create_bug_summary res in
    let benchmark_summary = create_benchmark_summary res in
    let msg =
      "Summary:\n"
      ^ sprintf "- Input file: %s\n" !input_file
      ^ assertion_summary ^ bug_summary ^ runtime_summary ^ benchmark_summary
      ^ sprintf "- Total runtime: %.2fs" total_time in
    println ~mtype:"" ~always:true ~autoformat:false ~ruler:`Long msg
;;

let handle_system_signals () =
  let handle_interrupt_signal (signal : Signal.t) : unit =
    if is_debug_mode ()
    then (
      let _ = flush_all () in
      let _ = println "\nReceived a keyboard interrupted signal!" in
      let _ = println "\nBacktrace:" in
      let _ = println (String.hindent 2 Printexc.get_backtrace ()) in
      exit 1)
    else exit 1 in
  (* handle system signals manually *)
  Signal.Expert.handle Signal.int handle_interrupt_signal
;;

let analyze_program (prog : CI.program) : analysis_result =
  match prog with
  | CI.Llprog prog ->
    let _ = printp "Work mode: " pr_work_mode !work_mode in
    let num_assertions = AS.count_all_assertions prog in
    let _ = print ("Found total assertions: " ^ pr_int num_assertions) in
    (match !work_mode with
    | WkmDFA -> RDfa (DA.analyze_program prog)
    | WkmSymExec -> RSymexec (SE.analyze_program prog)
    | WkmNoAnalysis ->
      let _ = print "No analysis mode is performed!" in
      RNone
    | _ -> RNone)
  | CI.Slprog prog ->
    let _ = SE.verify_program prog in
    RNone
;;

let analyze_input_file (filename : string) : analysis_result =
  let _ = printf "Analyze input file: %s" filename in
  let prog = CP.compile_input_file filename in
  analyze_program prog
;;

let main () : unit =
  (* let _ = enable_release_mode_alias_analysis () in *)
  let _ = handle_system_signals () in
  let run_discover () =
    let _ = AG.parse_arguments () in
    let _ = init_environment () in
    let _ = print_discover_settings () in
    analyze_input_file !input_file in
  let res, time = Sys.track_runtime ~f:run_discover in
  let _ = print_analysis_summary res time in
  clean_environment ()
;;

(* finally, run main *)
let _ =
  try main ()
  with e ->
    let _ = printp "Exception occurred: " Exn.to_string e in
    let _ = print (Printexc.get_backtrace ()) in
    if not (is_debug_mode ())
    then print "To debug, run Discover again with additional '-d'.";
    exit 1
;;
