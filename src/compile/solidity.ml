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
  let _ = Sys.mkdir_if_not_exists output_dir in
  let _ =
    let _ = Sys.remove_if_exists output_dir in
    let cmd =
      [ !solang_path ]
      @ [ "-emit"; "llvm-bc" ]
      @ [ "-O"; "none" ]
      @ [ "-o"; output_dir ]
      @ [ "--target"; "ewasm" ]
      @ String.split ~on:' ' !solang_user_options in
    let _ = debug (String.concat ~sep:" " cmd) in
    PS.run_command cmd in
  let generated_files = Sys.ls_dir output_dir in
  let deploy_file =
    List.find
      ~f:(String.is_suffix ~suffix:"_deploy.bc")
      generated_files in
  match deploy_file with
  | None -> error "compile_solidity: no output file generated!"
  | Some deploy_file ->
    let output_file = output_dir ^ FN.dir_sep ^ deploy_file in
    BC.compile_bitcode [] filename output_file
;;
