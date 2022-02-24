(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Core
open Llir
module DF = Dataflow
module SP = Set.Poly
module MP = Map.Poly

module TaintData = struct
  type t = (instr, instr) MP.t
end

module TaintTransfer : DF.ForwardDataTransfer with type t = TaintData.t =
struct
  include TaintData
  include DF.MakeDefaultEnv (TaintData)

  let analysis = DfaTaint

  (* TODO: Implement these functions *)

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  (* constants *)

  let least_data : t = MP.empty

  (* printers *)

  let pr_data (d : t) =
    let pr_taint kv =
      let k, v = kv in
      let str_k = LL.string_of_llvalue (llvalue_of_instr k) in
      let str_v = LL.string_of_llvalue (llvalue_of_instr v) in
      "Variable: " ^ str_k ^ " : " ^ str_v ^ "\n" in
    let list_of_data = MP.to_alist d in
    let list_of_strs = List.map list_of_data ~f:pr_taint in
    String.concat list_of_strs
  ;;

  let pr_data_checksum = pr_data

  (* comparison*)

  let equal_data (d1 : t) (d2 : t) : bool = MP.equal Poly.equal d1 d2
  let lequal_data (d1 : t) (d2 : t) : bool = Poly.( <= ) d1 d2
  let copy_data d = d

  (* substitution *)

  let subst_data
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (d : t)
      : t
    =
    d
  ;;

  let merge_data ?(widen = false) (d1 : t) (d2 : t) : t =
    MP.merge
      ~f:(fun ~key i ->
        match i with
        | `Both (i1, i2) -> Some i1
        | `Left i1 -> Some i1
        | `Right i2 -> Some i2)
      d1 d2
  ;;

  let join_data (d1 : t) (d2 : t) : t = merge_data d1 d2
  let clean_irrelevant_info_from_data penv func (d : t) : t = d

  let clean_info_of_vars (input : t) (vs : values) : t =
    (* TODO: implement later *)
    input
  ;;

  let is_data_satisfied_predicate (d : t) (p : predicate) : bool = true
  let refine_data_by_predicate ?(widen = false) (d : t) (p : predicate) : t = d

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let need_widening func : bool = true
  let prepare_entry_func_input (penv : prog_env) func (input : t) : t = input

  let prepare_callee_input
      (penv : prog_env)
      instr
      func
      (args : values)
      (input : t)
      : t
    =
    input
  ;;

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    input, []
  ;;

  let prepare_thrown_exception_data (penv : prog_env) exn_ptr tinfo (input : t)
      : t
    =
    input
  ;;

  let compute_catch_exception_data (penv : prog_env) instr ptr (input : t) exn
      : t
    =
    input
  ;;

  let analyze_global (g : global) (input : t) : t =
    (* TODO: default behavior, implement later if necessary *)
    input
  ;;

  let analyze_instr
      ?(widen = false)
      (penv : prog_env)
      (fenv : func_env)
      (ins : instr)
      (input : t)
      : t
    =
    input
  ;;
end

module Analysis = struct
  include TaintTransfer
  include DF.MakeForwardDataFlow (TaintTransfer)
end
