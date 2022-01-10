(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module PS = Outils.Process
module BC = Bitcode
module LI = Llir
module LL = Llvm
module LT = Llinstrument

let find_entry_functions (prog : LI.program) : LI.funcs =
  List.fold_left
    ~f:(fun acc f -> if LI.is_func_main f then acc @ [ f ] else acc)
    ~init:[] prog.LI.prog_user_funcs
;;

let post_process_program (prog : LI.program) : LI.program =
  let _ = debug "Post-procedssing C/C++ program..." in
  let entry_funcs = find_entry_functions prog in
  let prog = { prog with LI.prog_entry_funcs = entry_funcs } in
  let _ =
    debugp ~header:true ~enable:!llvm_print_prog_info
      "PROGRAM INFORMATION AFTER POST-PROCESSING" LI.pr_program_info prog in
  prog
;;

let compile_program (input_file : string) : LI.program =
  let _ = debug ("Compiling file: " ^ input_file) in
  let basename = Filename.chop_extension (Filename.basename input_file) in
  let dirname = Filename.dirname input_file ^ Filename.dir_sep ^ "logs" in
  let _ = Sys.make_dir dirname in
  let output_filename = dirname ^ Filename.dir_sep ^ basename ^ ".raw.bc" in
  let _ =
    let _ = Sys.remove_file output_filename in
    (* TODO: possibly use the  llvm-normalizer as a pass of clang or opt?? *)
    let cmd =
      [ !clang_exe ] @ [ "-O0"; "-fno-rtti" ]
      @ [ "-Xclang"; "-disable-llvm-passes" ]
      @ [ "-Xclang"; "-disable-O0-optnone" ]
      @ [ "-Werror=implicit-function-declaration" ]
      @ [ "-emit-llvm"; "-c"; input_file ]
      @ [ "-o"; output_filename ]
      @ (if !report_source_code_name then [ "-g" ] else [])
      @ String.split ~on:' ' !clang_user_options in
    let _ = debugf "Compilation command: %s" (String.concat ~sep:" " cmd) in
    PS.run_command cmd in
  let prog =
    if !bug_annotation
    then (
      let annots = LT.extract_bug_annotations input_file in
      let llcontext = LL.create_context () in
      let llmem = LL.MemoryBuffer.of_file output_filename in
      let modul = Llvm_bitreader.parse_bitcode llcontext llmem in
      let _ = LT.instrument_bitcode annots input_file modul in
      let instrued_filename =
        dirname ^ Filename.dir_sep ^ basename ^ ".ins.bc" in
      let _ = LL.set_module_identifer modul instrued_filename in
      let _ =
        if !print_instrumented_prog && !mode_deep_debug
        then debugp ~ruler:`Long "Instrumented: " LL.string_of_llmodule modul
      in
      let _ =
        let instrued_file = open_out instrued_filename in
        let _ = Llvm_bitwriter.output_bitcode instrued_file modul in
        close_out instrued_file in
      BC.process_bitcode instrued_filename)
    else BC.process_bitcode output_filename in
  post_process_program prog
;;
