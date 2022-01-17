(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module PS = Outils.Process
module BC = Bitcode
module FN = Filename
module LI = Llir

let config_solang_compiler () : unit =
  let open Option.Let_syntax in
  let _ =
    match PS.run_command_get_output [ "which"; !solang_exe ] with
    | Ok res -> solang_exe := res
    | Error msg -> () in
  match PS.run_command_get_output [ !solang_exe; "--version" ] with
  | Ok output ->
    ignore
      (let%bind line = String.find_line_contain ~pattern:"version" output in
       let%bind version = String.slice_from ~pattern:"version" line in
       return (solang_version := version))
  | Error msg -> warning "Solang version not found!"
;;

let builtin_Solang_functions =
  [ "__init_heap";
    "__memset8";
    "__memset";
    "__memcpy8";
    "__memcpy";
    "__bzero8";
    "__be32toleN";
    "__beNtoleN";
    "__leNtobe32";
    "__leNtobeN";
    "vector_new";
    "__malloc";
    "vector_hash";
    "__memcmp";
    "concat";
    "__free";
    "__realloc";
    "__mul32";
    "bits";
    "bits128";
    "shl128";
    "shr128";
    "udivmod128";
    "sdivmod128";
    "bits256";
    "udivmod256";
    "sdivmod256";
    "bits512";
    "udivmod512";
    "sdivmod512";
    "hex_encode";
    "hex_encode_rev";
    "uint2hex";
    "uint2bin";
    "uint2dec";
    "uint128dec";
    "uint256dec"
  ]
;;

let find_entry_functions (prog : LI.program) : LI.funcs =
  List.fold_left
    ~f:(fun acc fn ->
      let fname = LI.func_name fn in
      if List.mem builtin_Solang_functions fname ~equal:String.equal
      then acc
      else fn :: acc)
    ~init:[] prog.LI.prog_user_funcs
;;

let post_process_program (prog : LI.program) : LI.program =
  let _ = debug "Post-procesing Solidity program..." in
  let entry_funcs = find_entry_functions prog in
  let prog = { prog with LI.prog_entry_funcs = entry_funcs } in
  let _ =
    debugp ~header:true ~enable:!llvm_print_prog_info
      "PROGRAM INFORMATION AFTER POST-PROCESSING" LI.pr_program_info prog in
  prog
;;

let compile_program (input_file : string) : LI.program =
  let _ = debug ("Compiling file: " ^ input_file) in
  let contract_name = FN.basename input_file in
  let output_dir =
    FN.dirname input_file ^ FN.dir_sep ^ "logs" ^ FN.dir_sep ^ contract_name
  in
  let _ = debug ("Output dir: " ^ output_dir) in
  let _ = Sys.remove_dir output_dir in
  let _ = Sys.make_dir output_dir in
  let _ =
    let user_options =
      if String.is_empty !solang_user_options
      then []
      else String.split ~on:' ' !solang_user_options in
    let cmd =
      [ !solang_exe; input_file ]
      @ [ "--emit"; "llvm-bc" ]
      @ [ "--no-constant-folding"; "--no-strength-reduce" ]
      @ [ "--no-dead-storage"; "--no-vector-to-slice" ]
      @ [ "--no-implicit-type-cast-check" ]
      @ [ "-O"; "none"; "--target"; "ewasm" ]
      @ [ "-o"; output_dir ] @ user_options in
    let _ = debugf "Compilation command: %s" (String.concat ~sep:" " cmd) in
    match PS.run_command cmd with
    | Ok () -> ()
    | Error log -> error ~log ("Failed to compile file: " ^ input_file) in
  let output_files = Sys.ls_dir output_dir in
  let deploy_files =
    List.filter ~f:(String.is_suffix ~suffix:"_deploy.bc") output_files in
  if List.is_empty deploy_files
  then error "Compile Solidity smart contract: no output file is generated!"
  else (
    debugp "Solang generated files: " (pr_items ~f:pr_str) deploy_files;
    let output_files =
      List.map ~f:(fun f -> output_dir ^ FN.dir_sep ^ f) deploy_files in
    printp "Solang: deployed bitcode: " (pr_items ~f:pr_str) output_files;
    let _ =
      if List.length output_files > 1
      then warning "Solang: need to handle multiple contracts in the same file"
    in
    let prog = BC.process_bitcode (List.hd_exn output_files) in
    post_process_program prog)
;;
