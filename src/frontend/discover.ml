(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module AG = Arguments
module AS = Assertion
module CI = Commonir
module DA = Dfanalyzer
module CP = Compile
module PS = Process
module SE = Symexec
module VS = Version

let print_discover_settings () =
  let _ = print ~always:true ("Checking file: " ^ !input_file) in
  let info =
    [ "Discover's settings:";
      "  Git revision: " ^ VS.get_current_revision ();
      "  LLVM version: " ^ llvm_version;
      "  llvm-clang: " ^ !clang_exe;
      "  llvm-opt: " ^ !opt_exe;
      "  llvm-discover-normalizer: " ^ !normalizer_exe;
      "  Z3 solver: " ^ !Z3.z3cmd ^ " (" ^ !Z3.z3version ^ ")"
    ] in
  debug (String.concat ~sep:"\n" info)
;;

let init_solvers () =
  let _ =
    match PS.run_command_get_output [ !Z3.z3cmd; "--version" ] with
    | Ok res -> Z3.z3version := res
    | Error msg ->
      let _ = debug2 "Checking Z3 command: " !Z3.z3cmd in
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
    let msg =
      "Summary:\n"
      ^ sprintf "- Input file: %s\n" !input_file
      ^ sprintf "- Valid assertions: %d\n" !num_valid_asserts
      ^ sprintf "- Invalid assertions: %d\n" !num_invalid_asserts
      ^ sprintf "- Analysis time: %.2fs\n" !analysis_time
      ^ sprintf "- Total runtime: %.2fs" !total_time
      ^ detailed_runtime in
    println ~marker:false ~always:true ~autoformat:false ~ruler:`Long msg
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

let analyze_program (prog : CI.program) : unit =
  match prog with
  | CI.Llprog prog ->
    let _ = hprint "Work mode: " pr_work_mode !work_mode in
    let num_assertions = AS.count_all_assertions prog in
    let _ = print ("Found total assertions: " ^ pr_int num_assertions) in
    (match !work_mode with
    | WkmDFA -> DA.analyze_program prog
    (* | WkmDFA -> DAP.analyze_program prog *)
    (* | WkmDFA -> DAO.analyze_program prog *)
    | WkmSymExec -> SE.analyze_program prog
    | WkmNoAnalysis -> print "No analysis mode is performed!"
    | _ -> ())
  | CI.Slprog prog -> SE.verify_program prog
;;

let analyze_input_file (filename : string) : unit =
  let _ = print ("Analyze input file: " ^ filename) in
  let prog = CP.compile_input_file filename in
  analysis_time := snd (Sys.track_runtime (fun () -> analyze_program prog))
;;

let main () : unit =
  (* let _ = enable_release_mode_alias_analysis () in *)
  let _ = handle_system_signals () in
  let run_discover () =
    let _ = AG.parse_arguments () in
    let _ = init_environment () in
    let _ = print_discover_settings () in
    analyze_input_file !input_file in
  let _ = total_time := snd (Sys.track_runtime run_discover) in
  let _ = print_analysis_summary () in
  clean_environment ()
;;

(* finally, run main *)
let _ =
  try main () with
  | EError (msg, log) ->
    let _ = eprint ("ERROR: " ^ msg) in
    if not (!release_mode || String.is_empty log)
    then (
      eprint ("Detailed message:\n\n" ^ String.prefix_line ~prefix:"  > " log);
      eprint ("Exception:\n\n" ^ String.hindent 2 Printexc.get_backtrace ()));
    exit 1
  | e ->
    if not !release_mode
    then (
      eprint "ERROR: an exception occurred!";
      eprint ("Exception: " ^ Exn.to_string e);
      eprint (Printexc.get_backtrace ()));
    if not (is_debug_mode ())
    then eprint "To debug, run Discover again with additional '-d'.";
    exit 1
;;
