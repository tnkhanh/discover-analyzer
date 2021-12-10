(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LT = LL.TypeKind
module LV = LL.ValueKind
module LO = LL.Opcode
module SP = Set.Poly

(* Include sub-modules *)

include Llast.AST
include Llast.Value
include Llast.Type
include Llast.Global
include Llast.Expr
include Llast.Instr
include Llast.Const
include Llast.Func
include Llast.Callable
include Llast.Block
include Llast.Path
include Llast.Loop
include Llast.Program
include Llprinter.PrinterPrimitives
include Llutils.IterUtils
include Llutils.MapUtils
include Llutils.FoldUtils
include Llutils.TypeUtils
include Llutils.ValueUtils
include Llutils.UseUtils
include Llutils.InstrUtils
include Llutils.GlobalUtils
include Llutils.BlockUtils
include Llutils.FuncUtils
include Llutils.PathUtils
include Llutils.Metadata
include Llutils.ProgramUtils
include Llutils.Substitution


(*******************************************************************
 ** more advanced printing
 *******************************************************************)

module Printing = struct
  let pr_block (blk : block) : string =
    let blkname = block_name blk in
    let sinstrs =
      blk
      |> map_instrs ~f:(String.hindent 2 pr_instr)
      |> String.concat ~sep:"\n" in
    (" " ^ blkname ^ ":\n")
    ^ String.replace_if_empty sinstrs ~replacer:"{Empty block}"
  ;;

  let pr_func (f : func) : string =
    let fname =
      sprintf "Function: %s %s(%s)"
        (pr_type (func_return_type f))
        (func_name f)
        (pr_args ~f:pr_typed_param (func_params f)) in
    let sblks =
      f |> map_blocks ~f:pr_block |> String.concat ~sep:"\n\n"
      |> String.replace_if_empty ~replacer:"{Empty function}" in
    fname ^ "\n" ^ sblks
  ;;

  let pr_module (m : bitcode_module) : string = LL.string_of_llmodule m

  let pr_program (prog : program) : string =
    let sglobals =
      prog.prog_globals
      |> List.map ~f:(String.hindent 2 (pr_global ~detailed:true))
      |> String.concat ~sep:"\n"
      |> String.prefix_if_not_empty ~prefix:"Globals:\n" in
    let sstructs =
      prog.prog_struct_types
      |> List.map ~f:(fun t -> "  " ^ pr_type t)
      |> String.concat ~sep:"\n"
      |> String.prefix_if_not_empty ~prefix:"Struct types:\n" in
    let funcs = prog.prog_init_funcs @ prog.prog_user_funcs in
    let sfuncs = funcs |> List.map ~f:pr_func |> String.concat ~sep:"\n\n" in
    (sglobals |> String.suffix_if_not_empty ~suffix:"\n\n")
    ^ (sstructs |> String.suffix_if_not_empty ~suffix:"\n\n")
    ^ sfuncs
  ;;

  let pr_caller_info (prog : program) : string =
    Hashtbl.fold
      ~f:(fun ~key:func ~data:callers acc ->
        let fname = func_name func in
        let caller_names =
          callers |> List.map ~f:func_name |> String.concat ~sep:", " in
        acc ^ "\n  " ^ fname ^ " <-- [" ^ caller_names ^ "]")
      ~init:"Caller graph:" prog.prog_func_data.pfd_callers
  ;;

  let pr_callee_info (prog : program) : string =
    Hashtbl.fold
      ~f:(fun ~key:f ~data:callees acc ->
        let fname = func_name f in
        let callee_names =
          callees |> List.map ~f:callable_name |> String.concat ~sep:", " in
        acc ^ "\n  " ^ fname ^ " --> [" ^ callee_names ^ "]")
      ~init:"Callee graph:" prog.prog_func_data.pfd_callees
  ;;

  let pr_func_call_info (prog : program) : unit =
    let pfd = prog.prog_func_data in
    let callees_info =
      "Function call information:\n"
      ^ Hashtbl.fold
          ~f:(fun ~key:f ~data:callees acc ->
            if List.is_empty callees
            then acc
            else
              (acc ^ "\n - " ^ func_name f ^ ":")
              ^ pr_items ~bullet:"    ->" ~f:callable_name callees)
          ~init:"" pfd.pfd_callees in
    let _ = debug callees_info in
    let callers_info =
      "====================================\n"
      ^ "* Information of function callers:\n"
      ^ Hashtbl.fold
          ~f:(fun ~key:f ~data:callers acc ->
            if List.is_empty callers
            then acc
            else
              (acc ^ "\n - " ^ func_name f ^ ":")
              ^ pr_items ~bullet:"    <-" ~f:func_name callers)
          ~init:"" pfd.pfd_callers in
    debug callers_info
  ;;

  let pr_program_info (prog : program) : string =
    sprintf " - Init functions: %s\n" (func_names prog.prog_init_funcs)
    ^ sprintf " - Library (no source code) functions: %s\n"
        (func_names prog.prog_lib_no_source_funcs)
    ^ sprintf " - Library (has source code) functions: %s\n"
        (func_names prog.prog_lib_has_source_funcs)
    ^ sprintf " - User functions: %s\n" (func_names prog.prog_user_funcs)
    ^ sprintf " - Entry functions: %s\n" (func_names prog.prog_entry_funcs)
    ^ sprintf " - Function call information: \n%s\n%s"
        (String.hindent 4 pr_caller_info prog)
        (String.hindent 4 pr_callee_info prog)
  ;;
end

include Printing
