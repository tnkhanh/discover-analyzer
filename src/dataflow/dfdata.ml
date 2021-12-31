(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LI = Llir
module BG = Bug
module MP = Map.Poly

(* analyses *)
module PT = Pointer.Analysis
module MS = Memsize.Analysis
module MT = Memtype.Analysis
module RG = Range.Analysis
module UD = Undef.Analysis

(*******************************************************************
 ** Data structures
 *******************************************************************)

type program_data =
  { pdata_program : LI.program;
    pdata_potential_bugs : BG.potential_bugs;
    pdata_env_memsize : MS.prog_env option;
    pdata_env_memtype : MT.prog_env option;
    pdata_env_pointer : PT.prog_env option;
    pdata_env_range : RG.prog_env option;
    pdata_env_undef : UD.prog_env option
  }

(*******************************************************************
 ** Constructor
 *******************************************************************)

let mk_program_data (prog : LI.program) : program_data =
  { pdata_program = prog;
    pdata_potential_bugs = [];
    pdata_env_memsize = None;
    pdata_env_memtype = None;
    pdata_env_pointer = None;
    pdata_env_range = None;
    pdata_env_undef = None
  }
;;

(*******************************************************************
 ** Utility
 *******************************************************************)

let is_stack_based_pointer
    (pdata : program_data)
    (ins : LI.instr)
    (ptr : LI.value)
    : bool
  =
  match pdata.pdata_env_memtype with
  | None -> false
  | Some penv ->
    let fn = LI.func_of_instr ins in
    (match Hashtbl.find penv.penv_func_envs fn with
    | None -> false
    | Some fenvs ->
      List.for_all
        ~f:(fun fenv ->
          match MT.get_instr_output fenv ins with
          | None -> false
          | Some info ->
            (match MP.find info (LI.expr_of_value ptr) with
            | Some Memtype.StackBased -> true
            | _ -> false))
        fenvs)
;;

let is_heap_based_pointer
    (pdata : program_data)
    (ins : LI.instr)
    (ptr : LI.value)
    : bool
  =
  match pdata.pdata_env_memtype with
  | None -> false
  | Some penv ->
    let fn = LI.func_of_instr ins in
    (match Hashtbl.find penv.penv_func_envs fn with
    | None -> false
    | Some fenvs ->
      List.for_all
        ~f:(fun fenv ->
          match MT.get_instr_output fenv ins with
          | None -> false
          | Some info ->
            (match MP.find info (LI.expr_of_value ptr) with
            | Some Memtype.HeapBased -> true
            | _ -> false))
        fenvs)
;;
