(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
module LL = Llvm
module CI = Commonir
module OC = Llvm.Opcode
module LI = Llir
module LU = Llutils
module LN = Llnormalize
module LS = Llsimplify
module LT = Llinstrument
module LP = Llpass
module PS = Process
module SE = Symexec
module BC = Bitcode

(*********************************************************
 * LLVM PROGRAM
 **********************************************************)

type os_type =
  | Linux
  | MacOS
  | Win32
  | Cygwin
  | UnknOS

let get_os_type () =
  if String.equal Sys.os_type "Win32"
  then Win32
  else if String.equal Sys.os_type "Cygwin"
  then Cygwin
  else if String.equal Sys.os_type "Unix"
  then (
    let ic = Unix.open_process_in "uname" in
    let uname = input_line ic in
    let _ = close_in ic in
    if String.equal uname "Linux"
    then Linux
    else if String.equal uname "Darwin"
    then MacOS
    else UnknOS)
  else UnknOS
;;

let config_llvm_core_tools () =
  (* TODO:
     1. implement llvm path finding for Linux, MacOS, Windows
     2. Check clang, opt version by running `clang --version`,
        `opt --version` and check if they statisfy LLVM version *)
  let os = get_os_type () in
  match os with
  | Linux ->
    (* let _ = if String.equal !llvm_path "" then llvm_path := "/usr/bin/" in *)
    let clang_bin = "clang-" ^ llvm_version in
    (* let opt_bin = "opt-" ^ llvm_version in *)
    let opt_bin = "opt" in
    let _ = clang_path := !llvm_path ^ clang_bin in
    opt_path := !llvm_path ^ opt_bin
  | MacOS ->
    let llvm_root_path = "/usr/local/Cellar/llvm@" ^ llvm_version in
    let llvm_version_paths = Sys.ls_dir llvm_root_path in
    if List.is_empty llvm_version_paths
    then error "config_llvm_core_tools: not configured yet"
    else (
      let llvm_version_path = List.hd_exn llvm_version_paths in
      let _ = llvm_path := llvm_root_path ^ "/" ^ llvm_version_path in
      let _ = clang_path := !llvm_path ^ "/bin/clang" in
      opt_path := !llvm_path ^ "/bin/opt")
  | _ -> error "config_llvm_core_tools: not configured yet"
;;

let config_llvm_normalizer () =
  llvm_normalizer_path := project_path ^ "/" ^ !llvm_normalizer_path
;;

let config_toolchain () =
  let _ = config_llvm_core_tools () in
  let _ = config_llvm_normalizer () in
  Golang.config_golang_compiler ()
;;

let get_input_type (filename : string) =
  match !input_mode with
  | InpUnkn ->
    (match snd (Filename.split_extension filename) with
    | None -> InpUnkn
    | Some ext ->
      if List.exists ~f:(String.equal ext) file_ext_seplogic
      then InpSepLogic
      else if List.exists ~f:(String.equal ext) file_ext_bitcode
      then InpBitcode
      else if List.exists ~f:(String.equal ext) file_ext_llir
      then InpLlir
      else if List.exists ~f:(String.equal ext) file_ext_go
      then InpGolang
      else if List.exists ~f:(String.equal ext) file_ext_c_cpp
      then InpCCpp
      else if List.exists ~f:(String.equal ext) file_ext_solidity
      then InpSolidity
      else InpUnkn)
  | _ -> !input_mode
;;

let compile_input_file (filename : string) : CI.program =
  let input_type = get_input_type filename in
  match input_type with
  | InpSepLogic -> filename |> SE.compile_sep_logic |> CI.mk_seplogic_prog
  | InpBitcode -> filename |> BC.compile_bitcode [] "" |> CI.mk_llvm_prog
  | InpLlir -> filename |> BC.compile_llir |> CI.mk_llvm_prog
  | InpCCpp -> filename |> Ccpp.compile_c_cpp |> CI.mk_llvm_prog
  | InpGolang -> filename |> Golang.compile_golang |> CI.mk_llvm_prog
  | InpSolidity -> filename |> Solidity.compile_solidity |> CI.mk_llvm_prog
  | InpUnkn -> error2 "Unknown input type: " filename
;;
