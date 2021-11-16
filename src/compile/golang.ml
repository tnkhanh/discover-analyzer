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
module BC = Bitcode
module CI = Commonir
module DA = Dfanalyzer
module DF = Dataflow
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

let config_golang_compiler () =
  if String.equal !gollvm_path ""
  then (
    try
      let go_path = FileUtil.which "go" in
      gollvm_path := String.sub go_path ~pos:0 ~len:(String.length go_path - 2)
    with _ -> ());
  if (not (String.equal !gollvm_path ""))
     && not (String.is_suffix !gollvm_path ~suffix:"/")
  then gollvm_path := !gollvm_path ^ "/"
;;

let compile_golang (filename : string) : LI.program =
  let _ = debug2 "Compiling Go file: " filename in
  let _ = debug2 "gollvm_path: " !gollvm_path in
  let dirname = Filename.dirname filename ^ Filename.dir_sep ^ "logs" in
  let _ = Sys.make_dir dirname in
  let bitcode_filename = dirname ^ Filename.dir_sep ^ filename ^ ".raw.bc" in
  (* Code to compile Go file in OCaml

  let exec_filename = dirname ^ Filename.dir_sep ^ "a.out" in
  let go_build_cmd = [!gollvm_path ^ "go"; "build"; "-a"; "-work"; "-x";
                      "-o"; exec_filename; filename] in
  let go_build_output = PS.run_command_get_output go_build_cmd in
  match go_build_output with
  | POutput go_build_output_str ->
    let lines = String.split go_build_output_str ~on:'\n' in
    let work_line = List.find_exn lines ~f:(fun line ->
      String.is_prefix line ~prefix:"WORK=") in
    let _ = hdebug "Line starting with WORK=: " pr_str work_line in
    let goc_line =
      try
        List.find_exn lines ~f:(fun line ->
          String.is_substring line ~substring:"llvm-goc -c")
      with Not_found_s _ -> error ("Remove go build cache and old output and rerun") in
    let _ = hdebug "gollvm compiled with: " pr_str goc_line in
    let fixed_goc_line = String.substr_replace_all goc_line ~pattern:"$WORK"
      ~with_: (String.sub work_line ~pos:5 ~len:(String.length work_line - 5)) in
    let _ = hdebug "replaced $WORK in command: " pr_str fixed_goc_line in
    let cmd_replaced_work = (String.split fixed_goc_line ~on:' ') @ ["-emit-llvm"]  in
    let rec modify_output_file cmd =
      match cmd with
      | [] -> []
      | hd :: tl ->
        let newtl = if String.equal hd "-o" then
                      match tl with
                      | [] -> []
                      | hd2 :: tl2 -> bitcode_filename :: tl2
                    else modify_output_file tl in
        hd :: newtl in
    let cmd_to_run = modify_output_file cmd_replaced_work in
    let _ = PS.run_command cmd_to_run in
    compile_bitcode bitcode_filename
  | PError go_build_output_str ->
    error ("go build failed:\n" ^ go_build_output_str)

  *)
  let script_name = project_path ^ Filename.dir_sep ^ "gobuild.sh" in
  let go_build_output = dirname ^ Filename.dir_sep ^ filename ^ ".txt" in
  let _ =
    PS.run_command
      [ script_name;
        filename;
        bitcode_filename;
        !gollvm_path ^ "go";
        go_build_output
      ] in
  BC.compile_bitcode [] "" bitcode_filename
;;
