(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module PS = Extcore.Process
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

let enable_release_mode_alias_analysis () =
  let _ = release_mode := true in
  let _ = work_mode := WkmDFA in
  let _ = dfa_analyses := [ DfaPointer ] in
  let _ = dfa_mode := DfaInterProc in
  let _ = no_debug := true in
  let _ = no_print := true in
  let _ = print_concise_output := true in
  print_stats_prog := true
;;

let print_analysis_summary () =
  match !work_mode with
  | WkmNoAnalysis -> ()
  | _ ->
    let detailed_runtime =
      if (not !mode_debug) || List.is_empty !detailed_task_time
      then ""
      else
        List.fold_left
          ~f:(fun acc (task, time) ->
            acc ^ "\n- " ^ task ^ ": " ^ Printf.sprintf "%.2fs" time)
          ~init:"\n\nDetailed runtime:" !detailed_task_time in
    let assertion_summary =
      if !check_assert
      then
        sprintf "- Valid assertions: %d\n" !num_valid_asserts
        ^ sprintf "- Invalid assertions: %d\n" !num_invalid_asserts
      else "" in
    let bug_summary =
      if !find_bug
      then sprintf "- Detected bugs: %d\n" !num_detected_bugs
      else "" in
    let msg =
      "Summary:\n"
      ^ sprintf "- Input file: %s\n" !input_file
      ^ assertion_summary ^ bug_summary
      ^ sprintf "- Analysis time: %.2fs\n" !analysis_time
      ^ sprintf "- Total runtime: %.2fs" !total_time
      ^ detailed_runtime in
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
    let _ = hprint "Work mode: " pr_work_mode !work_mode in
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

let analyze_input_file (filename : string) : unit =
  let _ = print ("Analyze input file: " ^ filename) in
  let prog = CP.compile_input_file filename in
  let _, time = Sys.apply_track_runtime ~f:(fun () -> analyze_program prog) in
  analysis_time := time
;;

let main () : unit =
  (* let _ = enable_release_mode_alias_analysis () in *)
  let _ = handle_system_signals () in
  let run_discover () =
    let _ = AG.parse_arguments () in
    let _ = debugf "Testing debugf %s %d %d " "Hello debugf" 1 2 in
    let _ = debug "Testing NON debugf" in
    let _ = init_environment () in
    let _ = print_discover_settings () in
    analyze_input_file !input_file in
  let _, time = Sys.apply_track_runtime ~f:run_discover in
  let _ = total_time := time in
  let _ = print_analysis_summary () in
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
