(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir
module DF = Dataflow
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LA = List.Assoc
module BG = Bug
module Opt = Option
module SP = Set.Poly
module MP = Map.Poly
module II = Int_interval

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

type memtype =
  | StackBased
  | HeapBased
  | MemUnknown
  | StackOrHeap

let least_memtype = MemUnknown
let equal_memtype (m1 : memtype) (m2 : memtype) = m1 == m2

let lequal_memtype (m1 : memtype) (m2 : memtype) =
  match m1, m2 with
  | MemUnknown, _ -> true
  | _, StackOrHeap -> true
  | _ -> false
;;

let pr_memtype (m : memtype) =
  match m with
  | StackBased -> "StackBased"
  | HeapBased -> "HeapBased"
  | MemUnknown -> "MemUnknown"
  | StackOrHeap -> "StackOrHeap"
;;

let pr_memtype_concise (m : memtype) =
  match m with
  | StackBased -> "S"
  | HeapBased -> "H"
  | MemUnknown -> "U"
  | StackOrHeap -> "A"
;;

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module MemTypeData = struct
  (* Mapping pointer exprs to their allocated memory type. *)
  type t = (expr, memtype) MP.t
end

module MemTypeUtil = struct
  include MemTypeData

  let get_memtype (e : expr) (d : t) : memtype =
    match MP.find d e with
    | Some s -> s
    | None -> least_memtype
  ;;

  let combine_memtype (m1 : memtype) (m2 : memtype) : memtype =
    match m1, m2 with
    | MemUnknown, _ -> m2
    | _, MemUnknown -> m1
    | StackBased, StackBased -> StackBased
    | HeapBased, HeapBased -> HeapBased
    | _ -> StackOrHeap
  ;;
end

module MemTypeTransfer : DF.ForwardDataTransfer with type t = MemTypeData.t =
struct
  include MemTypeData
  include MemTypeUtil
  include DF.MakeDefaultEnv (MemTypeData)

  let analysis = DfaMemType

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  let least_data = MP.empty

  let pr_data (d : t) =
    let size_info = MP.to_alist ~key_order:`Increasing d in
    let res =
      pr_list_square
        ~f:(fun (e, s) -> pr_expr e ^ ": " ^ pr_memtype_concise s)
        size_info in
    "MemType: " ^ res
  ;;

  let pr_data_checksum = pr_data
  let equal_data (d1 : t) (d2 : t) = MP.equal equal_memtype d1 d2

  let lequal_data (d1 : t) (d2 : t) : bool =
    let diffs = MP.symmetric_diff d1 d2 ~data_equal:equal_memtype in
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
          errorp "memsize: subst_data: new value is duplicated: " pr_expr ne)
      ~init:MP.empty d
  ;;

  let merge_data ?(widen = false) (d1 : t) (d2 : t) : t =
    MP.merge
      ~f:(fun ~key:v d ->
        match d with
        | `Both (s1, s2) -> Some (combine_memtype s1 s2)
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

  let update_memtype (e : expr) (m : memtype) (d : t) : t =
    MP.change ~f:(fun _ -> Some m) d e
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
    let vins = llvalue_of_instr ins in
    let eins = expr_of_value vins in
    match instr_opcode ins with
    | LO.Unreachable -> least_data
    | LO.Alloca -> update_memtype eins StackBased input
    | LO.Call ->
      let func = callee_of_instr_call ins in
      if is_func_malloc func
      then update_memtype eins HeapBased input
      else if is_func_free func
      then update_memtype eins HeapBased input
      else input
    | LO.BitCast ->
      let mtyp = get_memtype (operand_expr ins 0) input in
      update_memtype eins mtyp input
    | LO.GetElementPtr ->
      let mtyp = get_memtype (operand_expr ins 0) input in
      update_memtype eins mtyp input
    | LO.PHI ->
      (* TODO: need alias analysis to clear off some variables
         overshadowing by PHI node *)
      let mtyp = ref (get_memtype (operand_expr ins 0) input) in
      let _ = debugp " PHI original: " pr_memtype !mtyp in
      for i = 1 to num_operands ins - 1 do
        let cmt = get_memtype (operand_expr ins i) input in
        let _ = debugp " PHI current range: " pr_memtype cmt in
        mtyp := combine_memtype !mtyp cmt
      done;
      let _ = debugp " PHI final: " pr_memtype !mtyp in
      update_memtype eins !mtyp input
    | _ -> input
  ;;
end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module Analysis = struct
  include MemTypeTransfer
  include DF.MakeForwardDataFlow (MemTypeTransfer)
  module MU = MemTypeUtil
  module MT = MemTypeTransfer

  let get_memtype (v : value) (d : t) : memtype =
    MU.get_memtype (expr_of_value v) d
  ;;

  let pr_memtype (m : memtype) = pr_memtype m
end
