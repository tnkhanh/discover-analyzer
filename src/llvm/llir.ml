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

include Llprogram.AST
include Llprogram.Predicate
include Llprogram.Value
include Llprogram.Type
include Llprogram.Global
include Llprogram.Expr
include Llprogram.Instr
include Llprogram.Const
include Llprogram.Func
include Llprogram.Callable
include Llprogram.Block
include Llprogram.Loop
include Llprinter.PrinterPrimitives
include Llutils.TypeUtils
include Llutils.ValueUtils
include Llutils.UseUtils
include Llutils.InstrUtils
include Llutils.GlobalUtils
include Llutils.BlockUtils
include Llutils.FuncUtils
include Llutils.PathUtils
include Llutils.Substitution
include Llutils.IterUtils
include Llutils.MapUtils
include Llutils.FoldUtils

(*******************************************************************
 ** operations with predicate and path condition
 *******************************************************************)

(** Module contains utility functions to process path condition *)

(*******************************************************************
 ** more utility operations
 *******************************************************************)

(** TRUNG: this function might be inefficient if it is used to compute
    reachable blocks of all blocks.
    TODO: need to think how to re-use the result of a succeeding block
    when computing the reachability of a preceding block.*)

let get_reachable_blocks (prog : program) (blk : block) : blocks =
  let rec compute_reachables (queue : blocks) (visited : blocks) =
    match queue with
    | [] -> visited
    | blk :: nqueue ->
      let nblks =
        blk |> get_succeeding_blocks prog
        |> List.map ~f:(fun sblk -> sblk.sblk_block)
        |> List.exclude ~f:(List.mem ~equal:( == ) visited)
        |> List.exclude ~f:(List.mem ~equal:( == ) nqueue) in
      let nqueue = nqueue @ nblks in
      let nvisited = visited @ [ blk ] in
      compute_reachables nqueue nvisited in
  let compute () =
    let sblks =
      blk |> get_succeeding_blocks prog
      |> List.map ~f:(fun sb -> sb.sblk_block) in
    compute_reachables sblks [] in
  Hashtbl.find_or_compute prog.prog_block_data.pbd_reachable_blocks ~f:compute
    ~key:blk
;;

(* TODO: can be optimized by compute for all functions at once *)
let get_reachable_funcs (prog : program) (f : func) : funcs =
  let equal = equal_func in
  let rec compute_reachables (queue : func list) (visited : funcs) =
    match queue with
    | [] -> visited
    | f :: nqueue ->
      let callees = get_func_callees prog f in
      let nfs = List.diff callees (nqueue @ visited) ~equal in
      let nqueue = List.concat_dedup nqueue nfs ~equal in
      let nvisited = List.insert_dedup visited f ~equal in
      compute_reachables nqueue nvisited in
  let compute () =
    let callees = get_func_callees prog f in
    compute_reachables callees [] in
  Hashtbl.find_or_compute prog.prog_func_data.pfd_reachable_funcs ~key:f
    ~f:compute
;;

let is_reachable_func prog (src : func) (dst : func) : bool =
  let funcs = get_reachable_funcs prog src in
  List.mem funcs dst ~equal:equal_func
;;

let is_reachable_block prog (src : block) (dst : block) : bool =
  let func1, func2 = func_of_block src, func_of_block dst in
  if equal_func func1 func2
  then (
    let blks = get_reachable_blocks prog src in
    List.mem blks dst ~equal:equal_block)
  else false
;;

let is_reachable_instr prog (src : instr) (dst : instr) : bool =
  let rec check_reachable_instr_same_block instr1 instr2 =
    if equal_instr instr1 instr2
    then true
    else (
      match instr_succ instr1 with
      | None -> false
      | Some instr' -> check_reachable_instr_same_block instr' instr2) in
  let blk1, blk2 = block_of_instr src, block_of_instr dst in
  if equal_block blk1 blk2
  then check_reachable_instr_same_block src dst
  else is_reachable_block prog blk1 blk2
;;

(*******************************************************************
 ** operations with programs
 *******************************************************************)

module Program = struct
  let get_all_funcs (m : bitcode_module) =
    fold_left_functions ~f:(fun acc f -> acc @ [ f ]) ~init:[] m
  ;;

  let get_lib_no_source_funcs (m : bitcode_module) =
    fold_left_functions
      ~f:(fun acc f -> if is_lib_no_source_func f then acc @ [ f ] else acc)
      ~init:[] m
  ;;

  let get_lib_has_source_funcs (m : bitcode_module) =
    fold_left_functions
      ~f:(fun acc f -> if is_lib_has_source_func f then acc @ [ f ] else acc)
      ~init:[] m
  ;;

  let get_user_funcs (m : bitcode_module) =
    fold_left_functions
      ~f:(fun acc f -> if is_user_func f then acc @ [ f ] else acc)
      ~init:[] m
  ;;

  let get_discover_funcs (m : bitcode_module) =
    fold_left_functions
      ~f:(fun acc f ->
        if is_discover_assertion_func f then acc @ [ f ] else acc)
      ~init:[] m
  ;;

  let get_init_funcs (m : bitcode_module) =
    fold_left_functions
      ~f:(fun acc f -> if is_init_func f then acc @ [ f ] else acc)
      ~init:[] m
  ;;

  let find_user_func (prog : program) (fname : string) : func option =
    List.find
      ~f:(fun func -> String.equal (func_name func) fname)
      prog.prog_user_funcs
  ;;

  let get_current_funcs_of_pointer (prog : program) (v : value) : funcs =
    match Hashtbl.find prog.prog_func_data.pfd_funcs_of_pointer v with
    | None -> []
    | Some funcs -> funcs
  ;;

  let update_funcs_of_pointer (prog : program) (v : value) (funcs : funcs) =
    let curr_funcs = get_current_funcs_of_pointer prog v in
    let new_funcs = List.concat_dedup curr_funcs funcs ~equal:equal_func in
    let _ = hdebug "update func pointer of: " pr_value v in
    let _ = hdebug "   new funcs: " func_names new_funcs in
    Hashtbl.set prog.prog_func_data.pfd_funcs_of_pointer ~key:v ~data:new_funcs
  ;;

  let mk_program_meta_data (filename : string) (modul : bitcode_module)
      : program_meta_data
    =
    { pmd_bitcode_filename = filename;
      pmd_source_filename = "<unknown>";
      pmd_llvalue_original_name = Hashtbl.create (module String);
      pmd_module_id = "<unknown>";
      pmd_data_layout = LL.data_layout modul;
      pmd_target_platform = LL.target_triple modul
    }
  ;;

  let mk_program_func_data (modul : bitcode_module) : program_func_data =
    { pfd_return_instr = Hashtbl.create (module FuncKey);
      pfd_callers = Hashtbl.create (module FuncKey);
      pfd_callees = Hashtbl.create (module FuncKey);
      pfd_reachable_funcs = Hashtbl.create (module FuncKey);
      pfd_loops = Hashtbl.create (module FuncKey);
      pfd_used_globals = Hashtbl.create (module FuncKey);
      pfd_func_call_graph = CG.create ();
      pfd_block_graph = Hashtbl.create (module FuncKey);
      pfd_funcs_of_pointer = Hashtbl.create (module ValueKey);
      pfd_funcs_of_type = Hashtbl.create (module TypeKey)
    }
  ;;

  let mk_program_loop_data (modul : bitcode_module) : program_loop_data =
    { pld_loop_updated_instr = Hashtbl.create (module InstrKey);
      pld_loop_head_instr = Hashtbl.create (module InstrKey);
      pld_innermost_loop_containing_block = Hashtbl.create (module BlockKey);
      pld_innermost_loop_containing_value = Hashtbl.create (module ValueKey)
    }
  ;;

  let mk_program_block_data (modul : bitcode_module) : program_block_data =
    { pbd_preceding_blocks = Hashtbl.create (module BlockKey);
      pbd_succeeding_blocks = Hashtbl.create (module BlockKey);
      pbd_incoming_pathcond = Hashtbl.create (module BlockKey);
      pbd_reachable_blocks = Hashtbl.create (module BlockKey)
    }
  ;;

  let mk_program (filename : string) (modul : bitcode_module) : program =
    let globals =
      LL.fold_left_globals (fun acc g -> acc @ [ mk_global g ]) [] modul in
    { prog_globals = globals;
      prog_struct_types = [];
      prog_all_funcs = get_all_funcs modul;
      prog_discover_funcs = get_discover_funcs modul;
      prog_lib_no_source_funcs = get_lib_no_source_funcs modul;
      prog_lib_has_source_funcs = get_lib_has_source_funcs modul;
      prog_user_funcs = get_user_funcs modul;
      prog_init_funcs = get_init_funcs modul;
      prog_entry_funcs = [];
      prog_meta_data = mk_program_meta_data filename modul;
      prog_func_data = mk_program_func_data modul;
      prog_loop_data = mk_program_loop_data modul;
      prog_block_data = mk_program_block_data modul;
      prog_module_data = modul
    }
  ;;
end

include Program

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
