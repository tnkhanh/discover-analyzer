(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module PS = Outils.Process
module AG = Argument
module AS = Assertion
module CI = Commonir
module DA = Dfanalyzer
module SE = Symexec
module CP = Compile
module VS = Version

type analysis_result =
  | RDfa of DA.dfa_result
  | RSymexec of SE.symexec_result
  | RNone

let print_discover_settings () =
  let _ = print ~always:true ("Checking file: " ^ !input_file) in
  let info =
    [ "Discover's settings:";
      "  Git revision: " ^ VS.get_current_revision ();
      "  LLVM version: " ^ llvm_version;
      "  llvm-clang: " ^ !clang_exe;
      "  llvm-opt: " ^ !opt_exe;
      "  llvm-discover-normalizer: " ^ !normalizer_exe;
      "  Z3 solver: " ^ !Z3.z3exe ^ " (" ^ !Z3.z3version ^ ")"
    ] in
  debug (String.concat ~sep:"\n" info)
;;

let init_solvers () =
  let _ =
    match PS.run_command_get_output [ !Z3.z3exe; "--version" ] with
    | Ok res -> Z3.z3version := res
    | Error msg ->
      let _ = debug ("Checking Z3 command: " ^ !Z3.z3exe) in
      error "Z3 solver not found!" in
  ()
;;

let read_user_configuration () : unit =
  let root_dir = Filename.dirname Sys.executable_name in
  let config_file_path = root_dir ^ "/" ^ user_config_file in
  match Sys.file_exists config_file_path with
  | `No | `Unknown -> ()
  | `Yes ->
    let _ = print "Read user configuration" in
    let file_content = In_channel.read_all config_file_path in
    (match Yaml.of_string file_content with
    | Result.Error (`Msg msg) ->
      error ("Failed to read the configuration file: " ^ config_file_path)
    | Result.Ok config ->
      (try
         let gopath = Ezjsonm.find config [ "GOLLVM_BINARY_PATH" ] in
         let _ = gollvm_path := Ezjsonm.get_string gopath in
         print (" - GOLLVM: " ^ !gollvm_path)
       with exn -> ()))
;;

let init_environment () =
  let root_dir = Filename.dirname Sys.executable_name in
  let _ = lib_core_file := root_dir ^ "/" ^ !lib_core_file in
  let _ = read_user_configuration () in
  let _ = CP.config_toolchain () in
  let _ = init_solvers () in
  if !skip_analysis then work_mode := WkmNoAnalysis
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
  | RDfa rdfa ->
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
  else ""
;;

let create_bug_summary (res : analysis_result) : string =
  if !find_bug
  then (
    match res with
    | RDfa rdfa ->
      let total_bugs =
        sprintf "- Detected bugs: %d\n" rdfa.dfa_num_detected_bugs in
      let detailed_bugs =
        List.fold_left
          ~f:(fun acc (bug_name, n) ->
            acc ^ "  + " ^ bug_name ^ ": " ^ pr_int n ^ "\n")
          ~init:"" rdfa.dfa_detailed_detected_bugs in
      total_bugs ^ detailed_bugs
    | _ -> "")
  else ""
;;

let print_analysis_summary (res : analysis_result) (total_time : float) : unit =
  match !work_mode with
  | WkmNoAnalysis -> ()
  | _ ->
    let assertion_summary = create_assertion_summary res in
    let runtime_summary = create_runtime_summary res in
    let bug_summary = create_bug_summary res in
    let msg =
      "Summary:\n"
      ^ sprintf "- Input file: %s\n" !input_file
      ^ assertion_summary ^ bug_summary ^ runtime_summary
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
  let _ = print ("Analyze input file: " ^ filename) in
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
    let _ = print ("Exception occurred: " ^ Exn.to_string e) in
    let _ = print (Printexc.get_backtrace ()) in
    if not (is_debug_mode ())
    then print "To debug, run Discover again with additional '-d'.";
    exit 1
;;
