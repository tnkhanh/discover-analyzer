(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module AG = Arguments
module AS = Assertion
module BG = Bug
module CI = Commonir
module DA = Dfanalyzer
module DF = Dataflow
module BC = Bitcode
module LI = Llir
module LL = Llvm
module LT = Llinstrument
module LU = Llutils
module PS = Process
module FN = Filename

let find_entry_functions (prog : LI.program) : LI.funcs =
  List.fold_left
    ~f:(fun acc f ->
      let vf = LI.llvalue_of_func f in
      if LL.linkage vf == LL.Linkage.Internal then acc @ [ f ] else acc)
    ~init:[] prog.LI.prog_user_funcs
;;

let preprocess_program (prog : LI.program) : LI.program =
  let _ = debug "Preprocess Solidity program..." in
  let entry_funcs = find_entry_functions prog in
  let prog = { prog with LI.prog_entry_funcs = entry_funcs } in
  prog
;;

let compile_program (filename : string) : LI.program =
  let _ = debug ("Compiling file: " ^ filename) in
  let contract_name = FN.basename filename in
  let output_dir =
    FN.dirname filename ^ FN.dir_sep ^ "logs" ^ FN.dir_sep ^ contract_name
  in
  let _ = debug2 "Output dir: " output_dir in
  let _ = Sys.remove_dir output_dir in
  let _ = Sys.make_dir output_dir in
  let _ =
    let user_options =
      if String.is_empty !solang_user_options
      then []
      else String.split ~on:' ' !solang_user_options in
    let cmd =
      [ !solang_exe; filename ] @ [ "--emit"; "llvm-bc" ]
      @ [ "-O"; "none"; "--target"; "ewasm" ]
      @ [ "--no-constant-folding"; "--no-strength-reduce" ]
      @ [ "--no-dead-storage"; "--no-vector-to-slice" ]
      @ [ "-o"; output_dir ] @ user_options in
    PS.run_command cmd in
  let generated_files = Sys.ls_dir output_dir in
  let _ = hdebug "Generated files: " (pr_list ~f:pr_str) generated_files in
  let deploy_file =
    List.find ~f:(String.is_suffix ~suffix:"_deploy.bc") generated_files in
  match deploy_file with
  | None -> error "compile_program: no output file generated!"
  | Some deploy_file ->
    let output_file = output_dir ^ FN.dir_sep ^ deploy_file in
    let _ = print2 "Output file: " output_file in
    let prog = BC.process_bitcode output_file in
    preprocess_program prog
;;
