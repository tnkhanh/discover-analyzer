(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module PS = Outils.Process
module BC = Bitcode
module CI = Commonir
module SE = Symexec

(*********************************************************
 * LLVM PROGRAM
 **********************************************************)

let config_llvm_normalizer () =
  let _ = normalizer_exe := project_path ^ "/" ^ !normalizer_exe in
  match PS.run_command_get_output [ !normalizer_exe; "--version" ] with
  | Ok output ->
    let output_lines = String.split_lines output in
    let version_line =
      List.find ~f:(String.is_substring ~substring:"version") output_lines
    in
    (match version_line with
    | Some line ->
      (match String.substr_index line ~pattern:"version" with
      | Some index ->
        let version = String.slice line index (String.length line) in
        normalizer_version := version
      | None -> ())
    | None -> ())
  | Error msg -> warning "Normalizer version not found!"
;;

let config_llvm_opt () =
  let _ =
    let opt_path =
      if String.is_empty !llvm_bin_path
      then !llvm_opt_exe
      else if String.is_suffix ~suffix:"/" !llvm_bin_path
      then !llvm_bin_path ^ !llvm_opt_exe
      else !llvm_bin_path ^ "/" ^ !llvm_opt_exe in
    match PS.run_command_get_output [ "which"; opt_path ] with
    | Ok res -> llvm_opt_exe := res
    | Error msg -> () in
  match PS.run_command_get_output [ !llvm_opt_exe; "--version" ] with
  | Error msg -> error ("Failed to check LLVM Opt version: " ^ msg)
  | Ok output ->
    if String.is_substring ~substring:("version " ^ llvm_version) output
    then ()
    else
      error
        ("Expect LLVM Opt " ^ llvm_version ^ " but found: \n"
       ^ String.indent 2 output)
;;

let config_llvm_dis () =
  let _ =
    let dis_path =
      if String.is_empty !llvm_bin_path
      then !llvm_dis_exe
      else if String.is_suffix ~suffix:"/" !llvm_bin_path
      then !llvm_bin_path ^ !llvm_dis_exe
      else !llvm_bin_path ^ "/" ^ !llvm_dis_exe in
    match PS.run_command_get_output [ "which"; dis_path ] with
    | Ok res -> llvm_dis_exe := res
    | Error msg -> () in
  match PS.run_command_get_output [ !llvm_dis_exe; "--version" ] with
  | Error msg -> error ("Failed to check LLVM Dis version: " ^ msg)
  | Ok output ->
    if String.is_substring ~substring:("version " ^ llvm_version) output
    then ()
    else
      error
        ("Expect LLVM Dis " ^ llvm_version ^ " but found: \n"
       ^ String.indent 2 output)
;;

let config_toolchain () =
  config_llvm_opt ();
  config_llvm_dis ();
  config_llvm_normalizer ();
  Clang.config_clang_compiler ();
  Solang.config_solang_compiler ();
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
  | InpSolidity -> filename |> Solang.compile_program |> CI.mk_llvm_prog
  | InpUnkn -> error ("Unknown input type: " ^ filename)
;;
