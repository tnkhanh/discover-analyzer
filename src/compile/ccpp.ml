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

let compile_c_cpp (filename : string) : LI.program =
  let _ = debug ("Compiling file: " ^ filename) in
  let basename = Filename.chop_extension (Filename.basename filename) in
  let dirname = Filename.dirname filename ^ Filename.dir_sep ^ "logs" in
  let _ = Sys.mkdir_if_not_exists dirname in
  let output_filename = dirname ^ Filename.dir_sep ^ basename ^ ".raw.bc" in
  let _ =
    let _ = Sys.remove_file_if_exists output_filename in
    let clang_all_options =
      let str_option = !clang_options ^ " " ^ !clang_extra_options in
      String.split ~on:' ' str_option in
    (* TODO: possibly use the  llvm-normalizer as a pass of clang or opt?? *)
    let llcontext = LL.create_context () in
    let cmd =
      [ !clang_path
      ; "-O0"
      ; "-fno-rtti"
      ; "-Xclang"
      ; "-disable-llvm-passes"
      ; "-Xclang"
      ; "-disable-O0-optnone"
      ; "-Werror=implicit-function-declaration"
      ; "-emit-llvm"
      ; "-c"
      ; filename
      ; "-o"
      ; output_filename
      ]
      @ (if !llvm_orig_source_name then [ "-g" ] else [])
      @ clang_all_options in
    let _ = debug (String.concat ~sep:" " cmd) in
    PS.run_command cmd in
  let ann_marks = LT.extract_ann_marks filename in
  BC.compile_bitcode ann_marks filename output_filename
;;
