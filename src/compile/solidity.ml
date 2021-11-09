(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
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
module FN = Filename

let compile_solidity (filename : string) : LI.program =
  let _ = debug ("Compiling file: " ^ filename) in
  let contract_name = FN.basename filename in
  let output_dir =
    FN.dirname filename ^ FN.dir_sep ^ "logs" ^ FN.dir_sep ^ contract_name in
  let _ = debug2 "Output dir: " output_dir in
  let _ = Sys.remove_dir output_dir in
  let _ = Sys.make_dir output_dir in
  let _ =
    let cmd =
      [ !solang_path; filename ]
      @ [ "--emit"; "llvm-bc" ]
      @ [ "-O"; "none"; "--target"; "ewasm" ]
      @ [ "-o"; output_dir ]
      @ String.split ~on:' ' !solang_user_options in
    let cmd = List.exclude ~f:String.is_empty cmd in
    let _ = debugs ["COMMAND: '"; (String.concat ~sep:" " cmd); "'"] in
    PS.run_command cmd in
  let generated_files = Sys.ls_dir output_dir in
  let _ = hdebug "Generated files: " sprint_string_list generated_files in
  let deploy_file =
    List.find
      ~f:(String.is_suffix ~suffix:"_deploy.bc")
      generated_files in
  match deploy_file with
  | None -> error "compile_solidity: no output file generated!"
  | Some deploy_file ->
    let output_file = output_dir ^ FN.dir_sep ^ deploy_file in
    let _ = print2 "Output file: " output_file in
    BC.compile_bitcode [] filename output_file
;;
