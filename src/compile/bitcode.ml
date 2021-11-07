(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Sprinter
open Printer
open Debugger
module LL = Llvm
module OC = Llvm.Opcode
module LI = Llir
module LU = Llutils
module LN = Llnormalize
module LS = Llsimplify
module LT = Llinstrument
module LP = Llpass
module PS = Process

let print_module_stats filename =
  if !print_stats_prog
  then (
    let llcontext = LL.create_context () in
    let llmem = LL.MemoryBuffer.of_file filename in
    let modul = Llvm_bitreader.parse_bitcode llcontext llmem in
    let _ = LU.print_pointer_stats modul in
    LL.MemoryBuffer.dispose llmem)
  else ()
;;

let process_module
    ann_marks
    source_name
    (filename : string)
    (modul : LL.llmodule)
    : LI.program
  =
  let _ = print2 "Simplifying bitcode: " filename in
  let _ = LN.rename_vars_and_params modul in
  let _ =
    if !llvm_simplify
    then (
      let _ =
        Sys.report_runtime ~task:"Time simplifying bitcode" (fun () ->
            LS.simplify_module filename modul) in
      let _ =
        Sys.report_runtime ~task:"Time instrumenting bitcode" (fun () ->
            LT.instrument_bitcode ann_marks source_name modul) in
      (* (fun () -> LT.instrument_bitcode filename modul) in *)
      if !export_bitcode
      then (
        let basename = Filename.chop_extension (Filename.basename filename) in
        let dirname = Filename.dirname filename in
        let fname_ir = dirname ^ Filename.dir_sep ^ basename ^ ".ll" in
        let _ = LL.print_module fname_ir modul in
        debug ("Export LLVM IR to: " ^ fname_ir))) in
  let prog =
    Sys.report_runtime ~task:"Time preparing core program" (fun () ->
        let _ = LN.check_normalization modul in
        LI.mk_program filename modul) in
  let _ = LP.update_program_info prog in
  let _ =
    if (not !print_concise_output) && !print_core_prog
    then hprint ~ruler:`Header "CORE BITCODE PROGRAM" LI.pr_program prog in
  (* let _ = hdebug "Call Graph: " LI.pr_callee_info prog in *)
  let _ = if !llvm_print_prog_info then LI.print_program_analysis_info prog in
  prog
;;

let optimize_bitcode (filename : string) : string =
  (* run mem2reg optimization to promote memory to registers *)
  let _ = print2 "Optimize bitcode: " filename in
  let basename = Filename.chop_extension (Filename.basename filename) in
  let dirname = Filename.dirname filename in
  let _ = Sys.mkdir_if_not_exists dirname in
  let opted_filename = dirname ^ Filename.dir_sep ^ basename ^ ".opt.bc" in
  let _ =
    let _ = Sys.remove_file_if_exists opted_filename in
    let opt_extra_options =
      if String.is_empty !opt_options
      then []
      else String.split ~on:' ' !opt_options in
    let cmd =
      [ !opt_path;
        "-mem2reg";
        (* "-instcombine" (\* simple algebraic simplification *\); *)
        (* "-consthoist"; "-constprop"; *)
        (* "--early-cse"; "--early-cse-memssa"; *)
        filename;
        "-o";
        opted_filename
      ]
      @ opt_extra_options in
    let _ = debug ("Running llvm-opt:\n" ^ String.concat ~sep:" " cmd) in
    PS.run_command cmd in
  let output_filename = dirname ^ Filename.dir_sep ^ basename ^ ".core.bc" in
  let _ =
    if !llvm_normalize
    then (
      let _ = Sys.remove_file_if_exists output_filename in
      let cmd =
        [ !llvm_normalizer_path; opted_filename; "-o"; output_filename ] in
      let _ =
        debug ("Running llvm-normalizer:\n" ^ String.concat ~sep:" " cmd) in
      PS.run_command cmd)
    else PS.run_command [ "cp"; opted_filename; output_filename ] in
  output_filename
;;


let compile_bitcode ann_marks source_name (filename : string) : LI.program =
  let _ = print_module_stats filename in
  let output_filename = optimize_bitcode filename in
  let llcontext = LL.create_context () in
  let llmem = LL.MemoryBuffer.of_file output_filename in
  let modul = Llvm_bitreader.parse_bitcode llcontext llmem in
  let _ =
    print_endline
      ("=======================================\n" ^ output_filename
       ^ LL.string_of_llmodule modul
       ^ "=======================================") in
  let _ = LL.MemoryBuffer.dispose llmem in
  let _ =
    if !print_input_prog
    then hprint ~ruler:`Long "ORIGINAL BITCODE MODULE" LI.pr_module modul in
  process_module ann_marks source_name output_filename modul
;;

let compile_llir (filename : string) : LI.program =
  let llcontext = LL.create_context () in
  let llmem = LL.MemoryBuffer.of_file filename in
  let modul = Llvm_irreader.parse_ir llcontext llmem in
  process_module [] "" filename modul
;;
