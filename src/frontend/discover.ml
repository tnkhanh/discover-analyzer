(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

module AG = Arguments
module AS = Assertion
module BG = Bug
module CI = Commonir
module DA = Dfanalyzer
module DAO = Dfanalyzeroo
module DF = Dataflow
module LC = Llcompile
module LI = Llir
module LL = Llvm
module LU = Llutils
module NO = Normalize
module PS = Process
module PV = Prover
module SA = Slast
module SE = Symexec
module SI = Slir
module TF = Transform
module TI = Typeinfer
module VS = Version

let print_discover_settings () =
  let _ = print ~always:true ("Checking file: " ^ !input_file) in
  let info = "Discover's settings:" ^
             "\n  Git revision: " ^ (VS.get_current_revision ()) ^
             "\n  LLVM version: " ^ llvm_version ^
             "\n  llvm-clang: " ^ !clang_path ^
             "\n  llvm-opt: " ^ !opt_path ^
             "\n  llvm-discover-normalizer: " ^ !llvm_normalizer_path ^
             "\n  Z3 solver: " ^ !Z3.z3cmd ^ " (" ^ !Z3.z3version ^ ")"in
  debug info

let init_solvers () =
  let _ = match PS.run_command_get_output [!Z3.z3cmd; "--version"] with
    | PS.POutput res -> Z3.z3version := res
    | PS.PError _ ->
      let _ = hdebug "Checking Z3 command: " pr_str !Z3.z3cmd in
      error "Z3 solver not found!" in
  ()

let read_user_configuration () : unit =
  let root_dir = Filename.dirname Sys.executable_name in
  let config_file_path = root_dir ^ "/" ^ user_config_file in
  match Sys.file_exists config_file_path with
  | `No | `Unknown -> ()
  | `Yes ->
    let _ = print "Read user configuration" in
    let file_content = In_channel.read_all config_file_path in
    match Yaml.of_string file_content with
    | Result.Error (`Msg msg) ->
      error ("Failed to read the configuration file: " ^ config_file_path)
    | Result.Ok config ->
      try
        let gopath = Ezjsonm.find config ["GOLLVM_BINARY_PATH"] in
        let _ = gollvm_path := (Ezjsonm.get_string gopath) in
        print (" - GOLLVM: " ^ !gollvm_path)
      with exn -> ()

let init_environment () =
  let root_dir = Filename.dirname Sys.executable_name in
  let _ = lib_core_file := root_dir ^ "/" ^ !lib_core_file in
  let _ = read_user_configuration () in
  let _ = LC.config_toolchain () in
  let _ = init_solvers () in
  if !skip_analysis then work_mode := WkmNoAnalysis

let clean_environment () =
  Smt.stop_solver ()

let enable_release_mode_alias_analysis () =
  let _ = release_mode := true in
  let _ = work_mode := WkmDFA in
  let _ = dfa_analyses := [DfaPointer] in
  let _ = dfa_mode := DfaInterProc in
  let _ = no_debug := true in
  let _ = no_print := true in
  let _ = print_concise_output := true in
  print_stats_prog := true

let get_input_type (filename: string) =
  match !input_mode with
  | InpUnkn -> (match snd (Filename.split_extension filename) with
    | None -> InpUnkn
    | Some ext ->
      if List.exists ~f:(String.equal ext) file_ext_seplogic then
        InpSepLogic
      else if List.exists ~f:(String.equal ext) file_ext_bitcode then
        InpBitcode
      else if List.exists ~f:(String.equal ext) file_ext_llir then
        InpLlir
      else if List.exists ~f:(String.equal ext) file_ext_go then
        InpGolang
      else if List.exists ~f:(String.equal ext) file_ext_c_cpp then
        InpCCpp
      else InpUnkn)
  | _ -> !input_mode

let compile_input_file (filename: string) : CI.program =
  let input_type = get_input_type filename in
  match input_type with
  | InpSepLogic -> filename |> SE.compile_sep_logic |> CI.mk_seplogic_prog
  | InpBitcode -> filename |> LC.compile_bitcode |> CI.mk_llvm_prog
  | InpLlir -> filename |> LC.compile_llir |> CI.mk_llvm_prog
  | InpCCpp -> filename |> LC.compile_c_cpp |> CI.mk_llvm_prog
  | InpGolang -> filename |> LC.compile_golang |> CI.mk_llvm_prog
  | InpUnkn -> herror "Unknown input type: " pr_str filename

let print_analysis_summary () =
  match !work_mode with
  | WkmNoAnalysis -> ()
  | _ ->
    let detailed_runtime =
      if not !mode_debug then ""
      else List.fold_left ~f:(fun acc (task, time) ->
          acc ^ "\n- " ^ task ^ ": " ^ (Printf.sprintf "%.2fs" time)
        ) ~init:"\n\nDetailed runtime:" !detailed_task_time in
    let summary =
      "\nSummary:" ^
      "\n- Input file: " ^ !input_file ^
      (* "\n- Detected bugs: " ^ (pr_int !num_of_bugs) ^ *)
      "\n- Valid assertions: " ^ (pr_int !num_valid_asserts) ^
      "\n- Invalid assertions: " ^ (pr_int !num_invalid_asserts) ^
      (* "\n- Sparse preparing time: " ^ (sprintf "%.2fs" !sparse_time) ^ *)
      "\n- Analysis time: " ^ (sprintf "%.2fs" !analysis_time) ^
      "\n- Total runtime: " ^ (sprintf "%.2fs" !total_time) ^
      detailed_runtime in
    print ~always:true ~format:false ~ruler:`Long (summary ^ "\n")

let handle_system_signals () =
  let handle_interrupt_signal (signal: Signal.t) : unit =
    if is_debug_mode () then (
      let _ = flush_all () in
      let _ = print_endline "\nReceived a keyboard interrupted signal!" in
      let _ = print_endline "\nBacktrace:" in
      let _ = print_endline (hpr_indent 2 Printexc.get_backtrace ()) in
      exit 1)
    else exit 1 in
  (* handle system signals manually *)
  Signal.Expert.handle Signal.int handle_interrupt_signal

let analyze_program (prog: CI.program) : unit =
  match prog with
  | CI.Llprog prog -> (
      let _ = hprint "Work mode: " pr_work_mode !work_mode in
      let num_assertions = AS.count_all_assertions prog in
      let _ = print ("Found total assertions: " ^ (pr_int num_assertions)) in
      match !work_mode with
      (* | WkmDFA -> DA.analyze_program_llvm prog *)
      | WkmDFA -> DAO.analyze_program_llvm prog
      | WkmSymExec -> SE.analyze_program_llvm prog
      | WkmNoAnalysis -> print "No analysis mode is performed!"
      | _ -> ())
  | CI.Slprog prog -> SE.analyze_program_seplog prog

let analyze_input_file (filename: string) : unit =
  let _ = print ("Analyze input file: " ^ filename) in
  let prog = compile_input_file filename in
  analysis_time := snd (track_runtime (fun () -> analyze_program prog))

let main () : unit =
  (* let _ = enable_release_mode_alias_analysis () in *)
  let _ = handle_system_signals () in
  let run_discover () =
    let _ = AG.parse_arguments () in
    let _ = init_environment () in
    let _ = print_discover_settings () in
    analyze_input_file !input_file in
  let _ = total_time := snd (track_runtime run_discover) in
  let _ = print_analysis_summary () in
  clean_environment ()

(* finally, run main *)
let _ =
  try main () with
  | EError (msg, log) ->
    let msg = "ERROR: " ^ msg in
    let msg =
      if !release_mode || String.is_empty log then msg
      else msg ^ "\n\n" ^ "Detailed message:\n\n" ^
           (pr_prefix ~prefix:"  > " log) in
    let msg =
      if !release_mode || String.is_empty log then msg
      else msg ^ "\n\n" ^ "Exception occurred:\n\n" ^
           (hpr_indent 2 Printexc.get_backtrace ()) in
    let _ = prerr_endline ("\n" ^ msg) in
    exit 1
  | e ->
    let msg =
      if !release_mode then ""
      else match is_debug_mode () with
        | true -> "\nERROR: an exception occurred!\n\n" ^
                  "Exception: " ^ (Exn.to_string e) ^ "\n" ^
                  (Printexc.get_backtrace ())
        | false -> "\nERROR: an exception occurred!\n" ^
                   "Exception: " ^ (Exn.to_string e) ^ "\n" ^
                   (Printexc.get_backtrace ()) ^ "\n\n" ^
                   "To debug, run Discover again with additional '-d'." in
    let _ = prerr_endline msg in
    exit 1
