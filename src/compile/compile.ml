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
  let open Option.Let_syntax in
  let _ = normalizer_exe := project_path ^ "/" ^ !normalizer_exe in
  match PS.run_command_get_output [ !normalizer_exe; "--version" ] with
  | Ok output ->
    ignore
      (let%bind line = String.find_line_contain ~pattern:"version" output in
       let%bind version = String.slice_from ~pattern:"version" line in
       let version =
         String.substr_replace_all ~pattern:"version " ~with_:"v" version in
       return (normalizer_version := version))
  | Error msg -> warning "Normalizer version not found!"
;;

let config_llvm_opt () =
  let open Option.Let_syntax in
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
  | Ok output ->
    ignore
      (let%bind line = String.find_line_contain ~pattern:"version" output in
       let%bind version = String.slice_from ~pattern:"version" line in
       let version =
         String.substr_replace_all ~pattern:"version " ~with_:"v" version in
       return (llvm_opt_version := version))
  | Error msg -> warning "Llvm-opt version not found!"
;;

let config_llvm_dis () =
  let open Option.Let_syntax in
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
  | Ok output ->
    ignore
      (let%bind line = String.find_line_contain ~pattern:"version" output in
       let%bind version = String.slice_from ~pattern:"version" line in
       let version =
         String.substr_replace_all ~pattern:"version " ~with_:"v" version in
       return (llvm_dis_version := version))
  | Error msg -> warning "Llvm-dis version not found!"
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
