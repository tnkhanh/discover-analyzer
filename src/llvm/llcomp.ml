(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

module LL = Llvm
module OC = Llvm.Opcode
module LI = Llir
module LU = Llutils
module LN = Llnorm
module LS = Llsimp
module LP = Llpass
module PS = Process

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
  if String.equal Sys.os_type "Win32" then Win32
  else if String.equal Sys.os_type "Cygwin" then Cygwin
  else if String.equal Sys.os_type "Unix" then (
    let ic = Unix.open_process_in "uname" in
    let uname = input_line ic in
    let _ = close_in ic in
    if String.equal uname "Linux" then Linux
    else if String.equal uname "Darwin" then MacOS
    else UnknOS)
  else UnknOS

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
    if List.is_empty llvm_version_paths then
      error "config_llvm_core_tools: not configured yet"
    else (
      let llvm_version_path = List.hd_exn llvm_version_paths in
      let _ = llvm_path := llvm_root_path ^ "/" ^ llvm_version_path in
      let _ = clang_path := !llvm_path ^ "/bin/clang" in
      opt_path := !llvm_path ^ "/bin/opt")
  | _ -> error "config_llvm_core_tools: not configured yet"

let discover_path = Filename.realpath (Sys.argv.(0))
let project_path = Filename.dirname discover_path

let config_llvm_normalizer () =
  llvm_normalizer_path := project_path ^ "/" ^ !llvm_normalizer_path

let config_llvm_golang () =
  let _ = if String.equal !gollvm_path "" then
    try
      let go_path = FileUtil.which "go" in
      gollvm_path := String.sub go_path ~pos:0 ~len:(String.length go_path - 2)
    with _ -> () in
  if (not (String.equal !gollvm_path "")) &&
    not (String.is_suffix !gollvm_path ~suffix:"/") then
  gollvm_path := !gollvm_path ^ "/"

let config_toolchain () =
  let _ = config_llvm_core_tools () in
  let _ = config_llvm_normalizer () in
  let _ = config_llvm_golang () in
  ()

let print_module_stats filename =
  if !print_stats_prog then
    let llcontext = LL.create_context () in
    let llmem = LL.MemoryBuffer.of_file filename in
    let modul = Llvm_bitreader.parse_bitcode llcontext llmem in
    let _ = LU.print_pointer_stats modul in
    LL.MemoryBuffer.dispose llmem
  else ()

let process_module (filename: string) (modul: LL.llmodule) : LI.program =
  let _ = hprint "Simplifying bitcode: " pr_id filename in
  let _ = LN.rename_vars_and_params modul in
  let _ = if !llvm_simplify then
      let _ = report_runtime ~task:"Time simplifying bitcode"
                (fun () -> LS.simplify_module filename modul) in
      if !export_bitcode then (
        let basename = Filename.chop_extension (Filename.basename filename) in
        let dirname = Filename.dirname filename in
        let fname_ir = dirname ^ Filename.dir_sep ^ basename ^ ".ll" in
        let _ = LL.print_module fname_ir modul in
        hdebug "Export LLVM IR to: " pr_str fname_ir) in
  let prog = report_runtime ~task:"Time preparing core program"
               (fun () ->
                  let _ = LN.check_normalization modul in
                  LI.mk_program filename modul) in
  let _ = LP.update_program_info prog in
  let _ = if not !print_concise_output && !print_core_prog then
      hprint ~ruler:`Header "CORE BITCODE PROGRAM" LI.pr_program prog in
  (* let _ = hdebug "Call Graph: " LI.pr_callee_info prog in *)
  let _ = if !llvm_print_prog_info then
      LI.print_program_analysis_info prog in
  prog

let compile_llir (filename: string) : LI.program =
  let llcontext = LL.create_context () in
  let llmem = LL.MemoryBuffer.of_file filename in
      let modul = Llvm_irreader.parse_ir llcontext llmem in
  process_module filename modul

let optimize_bitcode (filename: string) : string =
  (* run mem2reg optimization to promote memory to registers *)
  let _ = hprint "Optimize bitcode: " pr_id filename in
  let basename = Filename.chop_extension (Filename.basename filename) in
  let dirname = Filename.dirname filename in
  let _ = mkdir_if_not_exists dirname in
  let opted_filename = dirname ^ Filename.dir_sep ^ basename ^ ".opt.bc" in
  let _ =
    let _ = remove_file_if_exists opted_filename in
    let opt_extra_options =
      if String.is_empty !opt_options then []
      else String.split ~on:' ' !opt_options in
    let cmd = [!opt_path; "-mem2reg";
               (* "-instcombine" (\* simple algebraic simplification *\); *)
               (* "-consthoist"; "-constprop"; *)
               (* "--early-cse"; "--early-cse-memssa"; *)
               filename; "-o"; opted_filename] @
              opt_extra_options in
    let _ = debug ("Running llvm-opt:\n" ^
                   (String.concat ~sep:" " cmd)) in
    PS.run_command cmd in
  let output_filename = dirname ^ Filename.dir_sep ^ basename ^ ".core.bc" in
  let _ =
    if !llvm_normalize then
      let _ = remove_file_if_exists output_filename in
      let cmd = [!llvm_normalizer_path; opted_filename;
                 "-o"; output_filename] in
      let _ = debug ("Running llvm-normalizer:\n" ^
                     (String.concat ~sep:" " cmd)) in
      PS.run_command cmd
    else PS.run_command ["cp"; opted_filename; output_filename] in
  output_filename

let compile_bitcode (filename: string) : LI.program =
  let _ = print_module_stats filename in
  let output_filename = optimize_bitcode filename in
  let llcontext = LL.create_context () in
  let llmem = LL.MemoryBuffer.of_file output_filename in
  let modul = Llvm_bitreader.parse_bitcode llcontext llmem in
  let _ = LL.MemoryBuffer.dispose llmem in
  let _ = if !print_input_prog then
      hprint ~ruler:`Long "ORIGINAL BITCODE MODULE" LI.pr_module modul in
  process_module output_filename modul

let compile_c_cpp (filename: string) : LI.program =
  let _ = hdebug "Compiling file: " pr_str filename in
  let basename = Filename.chop_extension (Filename.basename filename) in
  let dirname = (Filename.dirname filename) ^ Filename.dir_sep ^ "logs" in
  let _ = mkdir_if_not_exists dirname in
  let output_filename = dirname ^ Filename.dir_sep ^ basename ^ ".raw.bc" in
  let _ =
    let _ = remove_file_if_exists output_filename in
    let clang_all_options =
      let str_option = !clang_options ^ " " ^ !clang_extra_options in
      String.split ~on:' ' str_option in
    (* TODO: possibly use the  llvm-normalizer as a pass of clang or opt?? *)
    let llcontext = LL.create_context () in
    let cmd = [!clang_path; "-O0"; "-fno-rtti";
               "-Xclang"; "-disable-llvm-passes";
               "-Xclang"; "-disable-O0-optnone";
               "-Werror=implicit-function-declaration";
               "-emit-llvm"; "-c"; filename; "-o"; output_filename] @
              (if !llvm_orig_source_name then ["-g"] else []) @
              clang_all_options in
    let _ = debug (String.concat ~sep:" " cmd) in
    PS.run_command cmd in
  compile_bitcode output_filename

let compile_golang (filename: string) : LI.program =
  let _ = hdebug "Compiling Go file: " pr_str filename in
  let _ = hdebug "gollvm_path: " pr_str !gollvm_path in
  let dirname = (Filename.dirname filename) ^ Filename.dir_sep ^ "logs" in
  let _ = mkdir_if_not_exists dirname in
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
  let _ = PS.run_command
            [script_name; filename; bitcode_filename; !gollvm_path ^ "go";
              go_build_output] in
  compile_bitcode bitcode_filename
