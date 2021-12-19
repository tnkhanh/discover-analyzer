(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore

(*******************************************************************
 ** Auxiliary fucntions
 *******************************************************************)

let enable_only_bug_type (bug_type : bool ref) : unit =
  let _ = bug_all := false in
  bug_type := true
;;

let enable_concise_output () : unit = Report.disable_warning := true

(*******************************************************************
 ** Arguments management
 *******************************************************************)

let print_error msg =
  error (msg ^ "\n" ^ "Run '" ^ Sys.argv.(0) ^ " -h' for help!\n")
;;

let rec print_usage () =
  let arguments = if !release_mode then arguments_release else arguments_raw in
  let welcome_msg =
    let prog_exe = Sys.argv.(0) in
    if !release_mode
    then sprintf "Usage: %s [options] <input file>\n\n" prog_exe
    else
      "Discover - a tool that finds bugs in programs and smart contracts.\n\n"
      ^ sprintf "Usage: %s [options] <input file>\n\n" prog_exe in
  let args_msg =
    List.fold_left
      ~f:(fun acc (flags, doc, _) ->
        let opt = Printf.sprintf "  %-20s" (String.concat ~sep:", " flags) in
        let opt =
          if String.length opt > 22
          then opt ^ Printf.sprintf "\n  %-20s" ""
          else opt in
        let opt = opt ^ Printf.sprintf "  %s\n" (String.strip_newline doc) in
        acc ^ opt)
      ~init:"" arguments in
  let usage_msg = "\n" ^ welcome_msg ^ args_msg in
  let _ = print_endline usage_msg in
  exit 0

and arguments_release =
  [ ( [ "--clang-option" ],
      "Command options for Clang",
      Arg.Set_string clang_user_options )
  ]

and arguments_raw =
  [ (*--------------------------------------------------------
     * printing
     *--------------------------------------------------------*)
    ( [ "--pip"; "--print-input-program" ],
      "Print input program",
      Arg.Set print_input_prog );
    ( [ "--pcp"; "--print-core-program" ],
      "Print core program",
      Arg.Set print_core_prog );
    ( [ "--dis-pcp"; "--dis-print-core-program" ],
      "Disable printing core program",
      Arg.Clear print_core_prog );
    ( [ "--pinstrp"; "--print-instrumented-program" ],
      "Print instrumented bitcode program",
      Arg.Set print_instrumented_prog );
    [ "--pap" ], "Print analyzed program", Arg.Set print_analyzed_prog;
    ( [ "--psp"; "--print-prog-stats" ],
      "Print statistics of program",
      Arg.Set print_stats_prog );
    [ "--type" ], "Print type information of variables", Arg.Set print_type;
    ( [ "--dis-pap" ],
      "Turn off printing analyzed programs",
      Arg.Clear print_analyzed_prog );
    [ "--pco" ], "Print concise output", Arg.Unit enable_concise_output;
    [ "--pcd" ], "Print concise debug", Arg.Set print_concise_debug;
    [ "--no-debug" ], "No debugging", Arg.Set no_debug;
    [ "--no-print" ], "No printing", Arg.Set no_print;
    ( [ "--edf" ],
      "Export debugging information to file",
      Arg.Unit
        (fun () ->
          export_core_prog := true;
          export_cfg_prog := true;
          export_debug_info := true) );
    ( [ "--msg-source-code" ],
      "Report bug at source code",
      Arg.Set location_source_code_only );
    (*--------------------------------------------------------
     * input and compilation
     *--------------------------------------------------------*)
    ( [ "--input-horn" ],
      "Proving formulas",
      Arg.Unit (fun () -> input_mode := InpSepLogic) );
    ( [ "--input-llvm" ],
      "Analyzing LLVM bitcode",
      Arg.Unit (fun () -> input_mode := InpBitcode) );
    ( [ "--clang-option" ],
      "Command options for Clang",
      Arg.Set_string clang_user_options );
    ( [ "--solang-option" ],
      "Command options for Solang",
      Arg.Set_string solang_user_options );
    ( [ "--opt-option" ],
      "Command options for LLVM-Opt",
      Arg.Set_string opt_user_options );
    ( [ "--enable-instrument" ],
      "Instrument bug annotations from source code",
      Arg.Set enable_instrument );
    (*--------------------------------------------------------
     * analysis
     *--------------------------------------------------------*)
    [ "--dis-analysis" ], "Disable all analysis passes", Arg.Set skip_analysis;
    ( [ "--symexec" ],
      "Symbolic execution",
      Arg.Unit (fun () -> work_mode := WkmSymExec) );
    ( [ "--dataflow"; "--dfa" ],
      "Data Flow Analysis",
      Arg.Unit (fun () -> work_mode := WkmDFA) );
    ( [ "--dfa-range" ],
      "Perform the data-flow range analysis",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_sparse_analysis := false;
          dfa_analyses := !dfa_analyses @ [ DfaRange ]) );
    ( [ "--dfa-undef" ],
      "Perform the data-flow undef analysis",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_analyses := !dfa_analyses @ [ DfaUndef ]) );
    ( [ "--dfa-memsize" ],
      "Perform the data-flow memory size analysis",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_analyses := !dfa_analyses @ [ DfaMemsize ]) );
    ( [ "--dfa-pointer" ],
      "Perform the data-flow pointer analysis",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_pointer_conservative := false;
          dfa_analyses := !dfa_analyses @ [ DfaPointer ]) );
    ( [ "--dfa-pointer-conservative" ],
      "Perform the data-flow pointer analysis conservatively",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_pointer_conservative := true;
          dfa_analyses := !dfa_analyses @ [ DfaPointer ]) );
    ( [ "--dfa-pointer-full" ],
      "Perform the data-flow pointer analysis",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_analyses := !dfa_analyses @ [ DfaUndef; DfaPointer ]) );
    ( [ "--dfa-all-analysis" ],
      "Perform all data-flow analyses",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_analyses := [ DfaAllAnalyses ]) );
    ( [ "--dfa-auto-schedule" ],
      "Auto schedule DFA analyses by bug types",
      Arg.Unit
        (fun () ->
          work_mode := WkmDFA;
          dfa_analyses := [ DfaAutoSchedule ]) );
    ( [ "--dfa-func" ],
      "Function to be analyzed",
      Arg.String (fun s -> dfa_func_name := Some s) );
    ( [ "--dfa-intra" ],
      "Intra-procedural analysis",
      Arg.Unit (fun () -> dfa_mode := DfaIntraProc) );
    ( [ "--dfa-inter" ],
      "Inter-procedural analysis, from the main function",
      Arg.Unit (fun () -> dfa_mode := DfaInterProc) );
    ( [ "--dfa-context-split-phi" ],
      "Enable/disable exhaustively split context from PHI instruction",
      Arg.Bool (fun b -> dfa_context_split_phi := b) );
    ( [ "--dfa-path-sensitive" ],
      "Enable/disable path-insensitive analysis",
      Arg.Bool (fun b -> dfa_path_sensitive := b) );
    ( [ "--dfa-sparse" ],
      "Enable/disable sparse analysis",
      Arg.Bool (fun b -> dfa_sparse_analysis := b) );
    ( [ "--dfa-used-globals-selective" ],
      "Enable/disable selectively analyzing global vars",
      Arg.Bool (fun b -> dfa_used_globals_selective := b) );
    ( [ "--dfa-used-globals-in-func-ptrs" ],
      "Enable/disable compute used globals in function pointers",
      Arg.Bool (fun b -> dfa_used_globals_in_func_ptrs := b) );
    (*--------------------------------------------------------
     * bug passes
     *--------------------------------------------------------*)
    ( [ "--bug-integer-overflow" ],
      "Find integer-overflow bugs",
      Arg.Unit (fun () -> enable_only_bug_type bug_integer_overflow) );
    ( [ "--bug-integer-underflow" ],
      "Find integer-underflow bugs",
      Arg.Unit (fun () -> enable_only_bug_type bug_integer_underflow) );
    ( [ "--bug-memory-leak" ],
      "Find memory leak bugs",
      Arg.Unit (fun () -> enable_only_bug_type bug_memory_leak) );
    ( [ "--bug-null-pointer-deref" ],
      "Find null pointer dereference bugs",
      Arg.Unit (fun () -> enable_only_bug_type bug_null_pointer_deref) );
    ( [ "--bug-buffer-overflow" ],
      "Find buffer overflow bugs",
      Arg.Unit (fun () -> enable_only_bug_type bug_buffer_overflow) );
    ( [ "--bug-integer-all" ],
      "Find all integer bugs",
      Arg.Unit (fun () -> enable_only_bug_type bug_integer_all) );
    ( [ "--bug-memory-all" ],
      "Find all memory bugs",
      Arg.Unit (fun () -> bug_memory_all := true) );
    ( [ "--bug-all" ],
      "Find all bugs",
      Arg.Unit
        (fun () ->
          bug_integer_all := true;
          bug_memory_all := true) );
    (*--------------------------------------------------------
     * llvm options
     *--------------------------------------------------------*)
    ( [ "--llvm-source-name" ],
      "Enable LLVM mode_debug with original source info",
      Arg.Set llvm_orig_source_name );
    ( [ "--llvm-prog-info" ],
      "Enable printing program's information",
      Arg.Set llvm_print_prog_info );
    ( [ "--llvm-no-simplify" ],
      "No not simplify LLVM IR",
      Arg.Clear llvm_simplify );
    ( [ "--llvm-no-optimize" ],
      "Do not optimize LLVM IR",
      Arg.Clear llvm_optimize );
    ( [ "--llvm-no-normalize" ],
      "Do not normalize LLVM IR",
      Arg.Clear llvm_normalize );
    [ "--llvm-path" ], "Path to llvm", Arg.Set_string llvm_path;
    (*--------------------------------------------------------
     * gollvm options
     *--------------------------------------------------------*)
    ( [ "--gollvm-path" ],
      "Path to gollvm, e.g., /usr/bin if gollvm is /usr/bin/go",
      Arg.Set_string gollvm_path );
    (*--------------------------------------------------------
     * input and output export
     *--------------------------------------------------------*)
    [ "--export-entailments" ], "Export entailments", Arg.Set export_entailment;
    [ "--export-bitcode" ], "Export LLVM Bitcode", Arg.Set export_bitcode;
    [ "--ascii" ], "Print proof in ascii format", Arg.Set export_proof_ascii;
    (*--------------------------------------------------------
     * mode_debug and help
     *--------------------------------------------------------*)
    [ "-i" ], "Interactive", Arg.Set mode_interactive;
    ( [ "-d" ],
      "Debug",
      Arg.Unit
        (fun () ->
          mode_debug := true;
          print_core_prog := true) );
    ( [ "-dd" ],
      "Deep Debug",
      Arg.Unit
        (fun () ->
          mode_debug := true;
          mode_deep_debug := true;
          print_core_prog := true) );
    ( [ "--debug-function" ],
      "Specify target functions for debugging",
      Arg.String
        (fun s ->
          mode_debug_function := true;
          regex_debug_function := s) );
    ( [ "--debug-working-function" ],
      "Specify target working functions for debugging",
      Arg.String
        (fun s ->
          mode_debug_working_function := true;
          regex_debug_working_function := s) );
    [ "-h"; "-help"; "--help" ], "Print all options", Arg.Unit print_usage
  ]
;;

let parse_arguments () : unit =
  try
    let all_input_files = ref [] in
    let collect_input_file arg = all_input_files := arg :: !all_input_files in
    let arguments =
      List.fold_left
        ~f:(fun acc (flags, doc, spec) ->
          let args = List.map ~f:(fun flag -> flag, spec, doc) flags in
          acc @ args)
        ~init:[] arguments_raw in
    let _ = Arg.parse_argv Sys.argv arguments collect_input_file "" in
    match !all_input_files with
    | [] -> print_error "Input file is undefined!"
    | [ file ] -> input_file := file
    | _ -> print_error "Too many input files. Only one is allowed!"
  with
  | Arg.Bad _ -> print_error ("unknown option: " ^ Sys.argv.(!Arg.current))
  | Arg.Help msg -> raise Exit
;;
