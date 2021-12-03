(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module BC = Bitcode
module CI = Commonir
module PS = Process
module SE = Symexec

(*********************************************************
 * LLVM PROGRAM
 **********************************************************)

type os_type =
  | Linux
  | MacOS
  | Win32
  | Cygwin
  | UnkownOS

let pr_os_type (os : os_type) =
  match os with
  | Linux -> "Linux"
  | MacOS -> "MacOS"
  | Win32 -> "Windows 32bit"
  | Cygwin -> "Cygwin"
  | UnkownOS -> "Uknown OS"
;;

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
    else UnkownOS)
  else UnkownOS
;;

let verify_llvm_compiler () =
  let _ =
    match PS.run_command_get_output [ !clang_exe; "--version" ] with
    | Error msg -> error ("Failed to check Clang version: " ^ msg)
    | Ok output ->
      if String.is_substring ~substring:("version " ^ llvm_version) output
      then ()
      else
        error
          ("Expect Clang " ^ llvm_version ^ " but found: \n"
         ^ String.indent 2 output) in
  match PS.run_command_get_output [ !opt_exe; "--version" ] with
  | Error msg -> error ("Failed to check LLVM Opt version: " ^ msg)
  | Ok output ->
    if String.is_substring ~substring:("version " ^ llvm_version) output
    then ()
    else
      error
        ("Expect LLVM Opt " ^ llvm_version ^ " but found: \n"
       ^ String.indent 2 output)
;;

let config_llvm_normalizer () =
  normalizer_exe := project_path ^ "/" ^ !normalizer_exe
;;

let config_toolchain () =
  let _ = verify_llvm_compiler () in
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
  | InpBitcode -> filename |> BC.process_bitcode |> CI.mk_llvm_prog
  | InpLlir -> filename |> BC.compile_program |> CI.mk_llvm_prog
  | InpCCpp -> filename |> Clang.compile_program |> CI.mk_llvm_prog
  | InpGolang -> filename |> Golang.compile_program |> CI.mk_llvm_prog
  | InpSolidity -> filename |> Solidity.compile_program |> CI.mk_llvm_prog
  | InpUnkn -> error ("Unknown input type: " ^ filename)
;;
