(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
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

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

module SizeDomain = struct

  type size = {
    size_min : int;             (* minimum size in bytes, inclusive *)
    size_max : int;             (* maximum size in bytes, inclusive *)
  }

  let pr_size s =
    "[" ^ (pr_int s.size_min) ^ "," ^ (pr_int s.size_max) ^ "]"

  let mk_size_range min max =
    { size_min = min;
      size_max = max; }

  let mk_size_const c =
    mk_size_range c c

  let least_size = mk_size_const 0

  let equal_size (a: size) (b: size) =
    (a.size_min = b.size_min) && (a.size_max = b.size_max)

  let lequal_size (a: size) (b: size) =
    (a.size_min <= b.size_min) && (a.size_max <= b.size_max)

  let union_size (a: size) (b: size) =
    mk_size_range (min a.size_min b.size_min) (max a.size_max b.size_max)

  let intersect_size (a: size) (b: size) =
    mk_size_range (max a.size_min b.size_min) (min a.size_max b.size_max)

  let widen_size (a: size) (b: size) =
    union_size a b

end;;

module SD = SizeDomain

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module MemsizeData = struct

  type t = (llvalue * SD.size) list      (* maintained as a sorted list *)

end

module SizeTransfer : DF.ForwardDataTransfer = struct

  include MemsizeData
  include DF.DataUtilGenerator(MemsizeData)

  let analysis = DfaMemsize

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  let least_data = []


  let pr_data d =
    pr_list_square (fun (v, s) -> (pr_value v) ^ ": " ^ (SD.pr_size s)) d

  let pr_data_checksum = pr_data

  let rec equal_data (d1: t) (d2: t) =
    match d1, d2 with
    | [], [] -> true
    | (v1, s1)::nd1, (v2, s2)::nd2 ->
      (equal_value v1 v2) && (SD.equal_size s1 s2) &&
      (equal_data nd1 nd2)
    | _ -> false

  let lequal_data (a: t)  (b: t) : bool =
    let rec leq xs ys = match xs, ys with
      | [], [] -> true
      | [], _ | _, [] -> false
      | (xv, xr)::nxs, (yv, yr)::nys ->
        if (equal_llvalue xv yv) && SD.lequal_size xr yr then leq nxs nys
        else false in
    leq a b

  let copy_data d =
    d

  let subst_data ?(sstv: substv = []) ?(sstve: substve = [])
        ?(sste: subste = []) (d: t) : t =
    List.map ~f:(fun (v, s) -> (subst_value sstv v, s)) d

  let merge_data ?(widen=false) (a: t) (b: t) : t =
    let rec combine xs ys acc = match xs, ys with
      | [], _ -> acc @ ys
      | _, [] -> acc @ xs
      | (xv, xr)::nxs, (yv, yr)::nys ->
        let cmp = String.compare (pr_value xv) (pr_value yv) in
        if cmp = 0 then
          combine nxs nys (acc @ [xv, SD.union_size xr yr])
        else if cmp < 0 then
          combine nxs ys (acc @ [xv, xr])
        else
          combine xs nys (acc @ [yv, yr]) in
    combine a b []

  let clean_irrelevant_info_from_data prog func (d: t) : t =
    List.exclude ~f:(fun (v, s) -> is_local_llvalue v) d

  let clean_info_of_vars (input: t) (vs: llvalues) : t =
    (* TODO: implement later *)
    input

  let is_data_satisfied_predicate (d: t) (p: predicate) : bool =
    true

  let refine_data_by_predicate ?(widen=false) (d: t) (p: predicate) : t =
    d

  (* FIXME: fix this later *)
  let join_data (a: t) (b: t) : t =
    merge_data a b

  let get_size (v: llvalue) (d: t) : SD.size =
    try LA.find_exn d ~equal:(==) v
    with _ -> SD.least_size

  let update_size (v: llvalue) (s: SD.size) (d: t) : t =
    let rec replace xs acc = match xs with
      | [] -> acc @ [(v, s)]
      | (xv, xr)::nxs ->
        let cmp = String.compare (pr_value v) (pr_value xv) in
        if cmp < 0 then (acc @ [(v, s)] @ xs)
        else if cmp = 0 then (acc @ [(v, s)] @ nxs)
        else replace nxs (acc @ [(xv, xr)]) in
    replace d []

  let combine_size (a: SD.size) (b: SD.size) =
    SD.union_size a b

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let prepare_callee_input penv instr callee args (input: t) : t =
    input

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    (input, [])

  let prepare_thrown_exception_data penv exn_ptr tinfo input : t =
    input

  let compute_catch_exception_data penv instr ptr input exn : t =
    input

  let need_widening func : bool =
    false

  let analyze_global (g: global) (input: t) : t =
    (* TODO: default behavior, implement later if necessary *)
    input

  let analyze_instr ?(widen=false) penv fenv (instr: instr) (input: t) : t =
    let vinstr = llvalue_of_instr instr in
    match instr_opcode instr with
    | LO.Unreachable -> least_data
    | LO.Call ->
      let func = callee_of_instr_call instr in
      if is_func_malloc func then
        let size = match int_of_const (operand instr 0) with
          | None -> 0
          | Some i -> i in
        update_size vinstr (SD.mk_size_const size) input
      else if is_func_free func then
        update_size vinstr (SD.mk_size_const 0) input
      else input
    | LO.BitCast ->
      let size = get_size (operand instr 0) input in
      update_size vinstr size input
    | LO.PHI ->
      (* TODO: need alias analysis to clear off some variables
         overshadowing by PHI node *)
      let ns = ref (get_size (operand instr 0) input) in
      let _ = hdebugc " PHI original: " SD.pr_size !ns in
      for i = 1 to (num_operands instr) - 1 do
        let cs = get_size (operand instr i) input in
        let _ = hdebugc " PHI current range: " SD.pr_size cs in
        ns := combine_size !ns cs
      done;
      let _ = hdebugc " PHI final: " SD.pr_size !ns in
      update_size vinstr !ns input
    | _ -> input

  (*******************************************************************
   ** Bug and assertions
   *******************************************************************)

  let get_size_of_inst (fenv: func_env) (instr: instr) : SD.size =
    match get_instr_output fenv instr with
    | None -> SD.least_size
    | Some data -> get_size (llvalue_of_instr instr) data

  let check_memory_leak (fenv: func_env) (mlk: BG.memory_leak) : ternary =
    match mlk.mlk_size with
    | None -> False
    | Some size -> True

  let check_bug (fenv: func_env) (bug: BG.bug) : ternary =
    match bug.BG.bug_type with
    | BG.MemoryLeak mlk -> check_memory_leak fenv mlk
    | _ -> Unkn

  let count_assertions (prog: program) : int =
    (* TODO: implement later if necessary *)
    0

  let check_assertions (penv: prog_env) func : int =
    (* TODO: implement later if necessary *)
    0

end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module MemsizeAnalysis = struct
  include SizeTransfer
  include DF.ForwardDataFlow(SizeTransfer)
end
