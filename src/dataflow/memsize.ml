(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir
module DF = Dataflow
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module BG = Bug
module Opt = Option
module MP = Map.Poly
module SP = Set.Poly
module II = Int_interval

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

type size = II.interval

let least_size = II.least_interval
let equal_size = II.equal_interval
let lequal_size = II.lequal_interval
let pr_size = II.pr_interval

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module SizeData = struct
  (* Mapping pointer exprs to their allocated memory size. *)
  type t = (expr, size) MP.t
end

module SizeUtil = struct
  include SizeData

  let get_size (e : expr) (d : t) : size =
    match MP.find d e with
    | Some s -> s
    | None -> least_size
  ;;

  let combine_size (a : size) (b : size) : size = II.union_interval a b
end

module SizeTransfer : DF.ForwardDataTransfer with type t = SizeData.t = struct
  include SizeData
  include SizeUtil
  include DF.MakeDefaultEnv (SizeData)

  let analysis = DfaMemSize

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  let least_data = MP.empty

  let pr_data (d : t) =
    let size_info = MP.to_alist ~key_order:`Increasing d in
    "MemSize: "
    ^ pr_list_square ~f:(fun (e, s) -> pr_expr e ^ ": " ^ pr_size s) size_info
  ;;

  let pr_data_checksum = pr_data
  let equal_data (d1 : t) (d2 : t) = MP.equal equal_size d1 d2

  let lequal_data (d1 : t) (d2 : t) : bool =
    let diffs = MP.symmetric_diff d1 d2 ~data_equal:equal_size in
    Sequence.for_all
      ~f:(fun diff ->
        match snd diff with
        | `Left _ -> false
        | `Unequal _ -> false
        | `Right _ -> true)
      diffs
  ;;

  let copy_data (d : t) = d

  let subst_data
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (d : t)
      : t
    =
    MP.fold
      ~f:(fun ~key:e ~data:d acc ->
        let ne = subst_expr ~sstv ~sstve ~sste e in
        match MP.add acc ~key:e ~data:d with
        | `Ok res -> res
        | `Duplicate ->
          herror "memsize: subst_data: new value is duplicated: " pr_expr ne)
      ~init:MP.empty d
  ;;

  let merge_data ?(widen = false) (d1 : t) (d2 : t) : t =
    MP.merge
      ~f:(fun ~key:v d ->
        match d with
        | `Both (s1, s2) -> Some (combine_size s1 s2)
        | `Left s1 -> Some s1
        | `Right s2 -> Some s2)
      d1 d2
  ;;

  let clean_irrelevant_info_from_data prog func (d : t) : t =
    MP.filter_keys
      ~f:(fun e ->
        let vs = collect_llvalue_of_expr e in
        List.for_all ~f:(fun v -> not (is_local_llvalue v)) vs)
      d
  ;;

  let clean_info_of_vars (input : t) (vs : values) : t =
    (* TODO: implement later *)
    input
  ;;

  let is_data_satisfied_predicate (d : t) (p : predicate) : bool = true
  let refine_data_by_predicate ?(widen = false) (d : t) (p : predicate) : t = d

  (* FIXME: fix this later *)

  let join_data (a : t) (b : t) : t = merge_data a b

  let update_size (e : expr) (s : size) (d : t) : t =
    MP.change ~f:(fun _ -> Some s) d e
  ;;

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let prepare_entry_func_input (penv : prog_env) func (input : t) : t = input

  let prepare_callee_input penv instr callee (args : values) (input : t) : t =
    input
  ;;

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    input, []
  ;;

  let prepare_thrown_exception_data penv exn_ptr tinfo input : t = input
  let compute_catch_exception_data penv instr ptr input exn : t = input
  let need_widening func : bool = false

  let analyze_global (g : global) (input : t) : t =
    (* TODO: default behavior, implement later if necessary *)
    input
  ;;

  let analyze_instr ?(widen = false) penv fenv (ins : instr) (input : t) : t =
    let prog = penv.penv_prog in
    let data_layout = get_program_data_layout prog in
    let vins = llvalue_of_instr ins in
    let eins = expr_of_value vins in
    match instr_opcode ins with
    | LO.Unreachable -> least_data
    | LO.Alloca ->
      let elem_typ = LL.element_type (type_of_instr ins) in
      let elem_size = size_of_type elem_typ data_layout in
      let num_elem =
        match int64_of_const (operand ins 0) with
        | None -> Int64.zero
        | Some i -> i in
      let buffer_size = Int64.(elem_size * num_elem) in
      update_size eins (II.mk_interval_int64 buffer_size) input
    | LO.Call ->
      let func = callee_of_instr_call ins in
      if is_func_malloc func
      then (
        let size =
          match int64_of_const (operand ins 0) with
          | None -> Int64.zero
          | Some i -> i in
        update_size eins (II.mk_interval_int64 size) input)
      else if is_func_free func
      then update_size eins (II.mk_interval_int64 Int64.zero) input
      else input
    | LO.BitCast ->
      let size = get_size (operand_expr ins 0) input in
      update_size eins size input
    | LO.PHI ->
      (* TODO: need alias analysis to clear off some variables
         overshadowing by PHI node *)
      let ns = ref (get_size (operand_expr ins 0) input) in
      let _ = hdebug " PHI original: " pr_size !ns in
      for i = 1 to num_operands ins - 1 do
        let cs = get_size (operand_expr ins i) input in
        let _ = hdebug " PHI current range: " pr_size cs in
        ns := combine_size !ns cs
      done;
      let _ = hdebug " PHI final: " pr_size !ns in
      update_size eins !ns input
    | _ -> input
  ;;
end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module Analysis = struct
  include SizeTransfer
  include DF.MakeForwardDataFlow (SizeTransfer)
  module SU = SizeUtil
  module ST = SizeTransfer

  let get_size (v : value) (d : t) : size = SU.get_size (expr_of_value v) d
  let pr_size (s : size) = pr_size s
end
