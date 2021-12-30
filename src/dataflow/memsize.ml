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
module ListA = List.Assoc
module ListO = List.OrderedList
module BG = Bug
module Opt = Option
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
  type t = (value * size) ListO.t
end

module SizeUtil = struct
  include SizeData

  let get_size (v : value) (d : t) : size =
    let elems = ListO.to_list d in
    match ListA.find elems v ~equal:( == ) with
    | Some s -> s
    | None -> least_size
  ;;
end

module SizeTransfer : DF.ForwardDataTransfer with type t = SizeData.t = struct
  include SizeData
  include SizeUtil
  include DF.MakeDefaultEnv (SizeData)

  let analysis = DfaMemsize

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  let least_data = []

  let pr_data d =
    "MemSize: "
    ^ pr_list_square ~f:(fun (v, s) -> pr_value v ^ ": " ^ pr_size s) d
  ;;

  let pr_data_checksum = pr_data

  let rec equal_data (d1 : t) (d2 : t) =
    match d1, d2 with
    | [], [] -> true
    | (v1, s1) :: nd1, (v2, s2) :: nd2 ->
      equal_value v1 v2 && equal_size s1 s2 && equal_data nd1 nd2
    | _ -> false
  ;;

  let lequal_data (a : t) (b : t) : bool =
    let rec leq xs ys =
      match xs, ys with
      | [], [] -> true
      | [], _ | _, [] -> false
      | (xv, xd) :: nxs, (yv, yd) :: nys ->
        if equal_value xv yv && lequal_size xd yd then leq nxs nys else false
    in
    leq a b
  ;;

  let copy_data d = d

  let subst_data
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (d : t)
      : t
    =
    List.map ~f:(fun (v, s) -> subst_value sstv v, s) d
  ;;

  let merge_data ?(widen = false) (a : t) (b : t) : t =
    let rec combine xs ys acc =
      match xs, ys with
      | [], _ -> acc @ ys
      | _, [] -> acc @ xs
      | (xv, xd) :: nxs, (yv, yd) :: nys ->
        let cmp = String.compare (pr_value xv) (pr_value yv) in
        if cmp = 0
        then combine nxs nys (acc @ [ xv, II.union_interval xd yd ])
        else if cmp < 0
        then combine nxs ys (acc @ [ xv, xd ])
        else combine xs nys (acc @ [ yv, yd ]) in
    combine a b []
  ;;

  let clean_irrelevant_info_from_data prog func (d : t) : t =
    List.exclude ~f:(fun (v, s) -> is_local_llvalue v) d
  ;;

  let clean_info_of_vars (input : t) (vs : values) : t =
    (* TODO: implement later *)
    input
  ;;

  let is_data_satisfied_predicate (d : t) (p : predicate) : bool = true
  let refine_data_by_predicate ?(widen = false) (d : t) (p : predicate) : t = d

  (* FIXME: fix this later *)

  let join_data (a : t) (b : t) : t = merge_data a b

  let update_size (v : value) (s : size) (d : t) : t =
    let rec replace xs acc =
      match xs with
      | [] -> acc @ [ v, s ]
      | (xv, xd) :: nxs ->
        let cmp = String.compare (pr_value v) (pr_value xv) in
        if cmp < 0
        then acc @ [ v, s ] @ xs
        else if cmp = 0
        then acc @ [ v, s ] @ nxs
        else replace nxs (acc @ [ xv, xd ]) in
    replace d []
  ;;

  let combine_size (a : size) (b : size) = II.union_interval a b

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
      update_size vins (II.mk_interval_int64 buffer_size) input
    | LO.Call ->
      let func = callee_of_instr_call ins in
      if is_func_malloc func
      then (
        let size =
          match int64_of_const (operand ins 0) with
          | None -> Int64.zero
          | Some i -> i in
        update_size vins (II.mk_interval_int64 size) input)
      else if is_func_free func
      then update_size vins (II.mk_interval_int64 Int64.zero) input
      else input
    | LO.BitCast ->
      let size = get_size (operand ins 0) input in
      update_size vins size input
    | LO.PHI ->
      (* TODO: need alias analysis to clear off some variables
         overshadowing by PHI node *)
      let ns = ref (get_size (operand ins 0) input) in
      let _ = hdebug " PHI original: " pr_size !ns in
      for i = 1 to num_operands ins - 1 do
        let cs = get_size (operand ins i) input in
        let _ = hdebug " PHI current range: " pr_size cs in
        ns := combine_size !ns cs
      done;
      let _ = hdebug " PHI final: " pr_size !ns in
      update_size vins !ns input
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

  let get_size (v : value) (d : t) : size = SU.get_size v d
  let pr_size = pr_size
end
