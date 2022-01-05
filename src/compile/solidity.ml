(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module PS = Outils.Process
module BC = Bitcode
module FN = Filename
module LI = Llir
module LL = Llvm

let find_entry_functions (prog : LI.program) : LI.funcs =
  List.fold_left
    ~f:(fun acc f ->
      let vf = LI.llvalue_of_func f in
      if LL.linkage vf == LL.Linkage.Internal then acc @ [ f ] else acc)
    ~init:[] prog.LI.prog_user_funcs
;;

let post_process_program (prog : LI.program) : LI.program =
  let _ = debug "Post-procesing Solidity program..." in
  let entry_funcs = find_entry_functions prog in
  let prog = { prog with LI.prog_entry_funcs = entry_funcs } in
  let _ =
    debugp ~header:true ~enable:!llvm_print_prog_info
      "PROGRAM INFORMATION AFTER POST-PROCESSING" LI.pr_program_info prog in
  prog
;;

let compile_program (input_file : string) : LI.program =
  let _ = debug ("Compiling file: " ^ input_file) in
  let contract_name = FN.basename input_file in
  let output_dir =
    FN.dirname input_file ^ FN.dir_sep ^ "logs" ^ FN.dir_sep ^ contract_name
  in
  let _ = debug ("Output dir: " ^ output_dir) in
  let _ = Sys.remove_dir output_dir in
  let _ = Sys.make_dir output_dir in
  let _ =
    let user_options =
      if String.is_empty !solang_user_options
      then []
      else String.split ~on:' ' !solang_user_options in
    let cmd =
      [ !solang_exe; input_file ]
      @ [ "--emit"; "llvm-bc" ]
      @ [ "-O"; "none"; "--target"; "ewasm" ]
      @ [ "-o"; output_dir ] @ user_options in
    let _ = debugf "Compilation command: %s" (String.concat ~sep:" " cmd) in
    match PS.run_command cmd with
    | Ok () -> ()
    | Error log ->
      error ~log ("Failed to compile file: " ^ input_file)
  in
  let generated_files = Sys.ls_dir output_dir in
  let _ = debugp "Generated files: " (pr_list ~f:pr_str) generated_files in
  let deploy_file =
    List.find ~f:(String.is_suffix ~suffix:"_deploy.bc") generated_files in
  match deploy_file with
  | None -> error "Compile Solidity program: no output file generated!"
  | Some deploy_file ->
    let output_file = output_dir ^ FN.dir_sep ^ deploy_file in
    let _ = print ("Output file: " ^ output_file) in
    let prog = BC.process_bitcode output_file in
    post_process_program prog
;;
