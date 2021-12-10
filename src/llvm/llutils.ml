(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llprogram
open Llir_operations
module SP = Set.Poly

module TypeUtils = struct
  let get_struct_types (m : bitcode_module) : datatype list =
    let all_stypes = ref Set.Poly.empty in
    let rec collect_struct_type typ =
      if is_type_struct typ
      then (
        match LL.struct_name typ with
        | None -> ()
        | Some _ -> all_stypes := Set.add !all_stypes typ)
      else (
        let subtypes = typ |> LL.subtypes |> Array.to_list in
        List.iter ~f:collect_struct_type subtypes) in
    let visit_global g =
      collect_struct_type (LL.type_of (llvalue_of_global g)) in
    let visit_instr i = collect_struct_type (LL.type_of (llvalue_of_instr i)) in
    let _ =
      iter_struct_module ~finstr:(Some visit_instr)
        ~fglobal:(Some visit_global) m in
    Set.to_list !all_stypes
  ;;
end

include TypeUtils

(*******************************************************************
 ** operations with value
 *******************************************************************)

(** Module contains utility functions to process LLVM Values *)

module ValueUtils = struct
  let collect_llvalue_of_expr (e : expr) : values =
    let rec collect e acc =
      match e with
      | Undef _ | Int64 _ | Float _ | String _ -> acc
      | Var v -> List.insert_dedup acc v ~equal:equal_value
      | OldE e -> collect e acc
      | Deref e -> collect e acc
      | ElemPtr (root, _, idxs) ->
        let acc1 = collect root acc in
        List.fold ~f:(fun acc2 e -> collect e acc2) ~init:acc1 idxs
      | Malloc e -> collect e acc
      | FuncRes f ->
        List.insert_dedup acc (llvalue_of_func f) ~equal:equal_value
      | Exn e -> collect e acc in
    collect e []
  ;;

  let collect_llvalue_of_predicate (p : predicate) : values =
    let equal = equal_value in
    let rec collect p =
      match p with
      | PBool _ -> []
      | PIcmp (_, lhs, rhs) -> List.dedup ~equal [ lhs; rhs ]
      | PFcmp (_, lhs, rhs) -> List.dedup ~equal [ lhs; rhs ]
      | PNeg p1 -> collect p1
      | PConj ps | PDisj ps ->
        List.fold_left
          ~f:(fun acc p1 -> List.concat_dedup acc (collect p1) ~equal)
          ~init:[] ps in
    collect p
  ;;
end

include ValueUtils

(*******************************************************************
 ** operations with use
 *******************************************************************)

(** Module contains utility functions to process LLVM Use *)

module UseUtils = struct
  let num_uses (v : value) : int = LL.fold_left_uses (fun acc _ -> acc + 1) 0 v

  let get_uses (v : value) : use list =
    LL.fold_left_uses (fun acc u -> acc @ [ u ]) [] v
  ;;

  let get_used_values (v : value) : value list =
    LL.fold_left_uses (fun acc u -> acc @ [ LL.used_value u ]) [] v
  ;;

  let get_users (v : value) : value list =
    LL.fold_left_uses (fun acc u -> acc @ [ LL.user u ]) [] v
  ;;
end

include UseUtils


(*******************************************************************
 ** operations with globals
 *******************************************************************)

(** Module contains utility functions to process global variables *)

module GlobalUtils = struct
  let index_of_global_name (g : global) : int =
    let gname = pr_global g in
    if String.is_prefix gname ~prefix:"g"
    then (
      let sindex = String.sub gname ~pos:1 ~len:(String.length gname - 1) in
      try Int.of_string sindex with _ -> -1)
    else -1
  ;;

  let compare_global_by_name blk1 blk2 : int =
    let idx1, idx2 = index_of_global_name blk1, index_of_global_name blk2 in
    if idx1 < idx2 then -1 else if idx1 > idx2 then 1 else 0
  ;;
end

include GlobalUtils

(*******************************************************************
 ** Utility functions to process instructions
 *******************************************************************)

(** Module contains utility functions to process instructions *)

module InstrUtils = struct
  (* General instruction utilities *)

  let is_instr_same_block (i1 : instr) (i2 : instr) : bool =
    let blk1 = block_of_instr i1 in
    let blk2 = block_of_instr i2 in
    equal_block blk1 blk2
  ;;

  let instr_succ (i : instr) : instr option =
    match LL.instr_succ (llvalue_of_instr i) with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_instr v)
  ;;

  let instr_pred (i : instr) : instr option =
    match LL.instr_pred (llvalue_of_instr i) with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_instr v)
  ;;

  let instr_begin (blk : block) : instr option =
    match LL.instr_begin blk with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_instr v)
  ;;

  (* Alloca *)

  let dst_of_instr_alloca (i : instr) : value =
    match instr_opcode i with
    | LO.Alloca -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr Alloca: " pr_instr i
  ;;

  (* Store *)

  let src_of_instr_store (i : instr) : value =
    match instr_opcode i with
    | LO.Store -> operand i 0
    | _ -> herror "src_of_instr: not an instr Store: " pr_instr i
  ;;

  let dst_of_instr_store (i : instr) : value =
    match instr_opcode i with
    | LO.Store -> operand i 1
    | _ -> herror "dst_of_instr: not an instr Store: " pr_instr i
  ;;

  (* Load *)

  let src_of_instr_load (i : instr) : value =
    match instr_opcode i with
    | LO.Load -> operand i 0
    | _ -> herror "src_of_instr: not an instr Load: " pr_instr i
  ;;

  let dst_of_instr_load (i : instr) : value =
    match instr_opcode i with
    | LO.Load -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr Load: " pr_instr i
  ;;

  (* InsertValue *)

  let src_of_instr_insertvalue (i : instr) : value =
    match instr_opcode i with
    | LO.InsertValue -> operand i 1
    | _ -> herror "src_of_instr: not an instr InsertValue: " pr_instr i
  ;;

  let dst_of_instr_insertvalue (i : instr) : value =
    match instr_opcode i with
    | LO.InsertValue -> operand i 0
    | _ -> herror "dst_of_instr: not an instr InsertValue: " pr_instr i
  ;;

  (* ExtractValue *)

  let src_of_instr_extractvalue (i : instr) : value =
    match instr_opcode i with
    | LO.ExtractValue -> operand i 0
    | _ -> herror "src_of_instr: not an instr ExtractValue: " pr_instr i
  ;;

  let dst_of_instr_extractvalue (i : instr) : value =
    match instr_opcode i with
    | LO.ExtractValue -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr ExtractValue: " pr_instr i
  ;;

  (* GetElementPointer *)

  let src_of_instr_gep (i : instr) : value =
    match instr_opcode i with
    | LO.GetElementPtr -> operand i 0
    | _ -> herror "src_of_instr: not an instr GEP: " pr_instr i
  ;;

  let dst_of_instr_gep (i : instr) : value =
    match instr_opcode i with
    | LO.GetElementPtr -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr GEP: " pr_instr i
  ;;

  let indexes_of_instr_gep (i : instr) : value list =
    let indexes = ref [] in
    for idx = 1 to num_operands i - 1 do
      indexes := !indexes @ [ operand i idx ]
    done;
    !indexes
  ;;

  (* GetElementPointer/ExtractValue *)

  let src_of_instr_gep_extract_value (i : instr) : value =
    match instr_opcode i with
    | LO.GetElementPtr -> operand i 0
    | LO.ExtractValue -> operand i 0
    | _ -> herror "src_of_instr: not an instr GEP/ExtractValue: " pr_instr i
  ;;

  let dst_of_instr_gep_extract_value (i : instr) : value =
    match instr_opcode i with
    | LO.GetElementPtr -> llvalue_of_instr i
    | LO.ExtractValue -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr GEP/ExtractValue: " pr_instr i
  ;;

  let indexes_of_instr_gep_extract_value (i : instr) : value list =
    let indexes = ref [] in
    for idx = 1 to num_operands i - 1 do
      indexes := !indexes @ [ operand i idx ]
    done;
    !indexes
  ;;

  (* BitCast *)

  let src_of_instr_bitcast (i : instr) : value =
    match instr_opcode i with
    | LO.BitCast -> operand i 0
    | _ -> herror "src_of_instr: not an instr BitCast: " pr_instr i
  ;;

  let dst_of_instr_bitcast (i : instr) : value =
    match instr_opcode i with
    | LO.BitCast -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr BitCast: " pr_instr i
  ;;

  let rec get_root_src_of_bitcast (v : value) : value =
    match LL.classify_value v with
    | LV.Instruction LO.BitCast -> get_root_src_of_bitcast (LL.operand v 0)
    | _ -> v
  ;;

  (* FuncRes *)

  let src_of_instr_return (i : instr) : value =
    match instr_opcode i with
    | LO.Ret -> operand i 0
    | _ -> herror "src_of_instr: not an instr FuncRes: " pr_instr i
  ;;

  (* SExt *)

  let src_of_instr_sext (i : instr) : value =
    match instr_opcode i with
    | LO.SExt -> operand i 0
    | _ -> herror "src_of_instr: not an instr SExt: " pr_instr i
  ;;

  let dst_of_instr_sext (i : instr) : value =
    match instr_opcode i with
    | LO.SExt -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr SExt: " pr_instr i
  ;;

  (* ZExt *)

  let src_of_instr_zext (i : instr) : value =
    match instr_opcode i with
    | LO.ZExt -> operand i 0
    | _ -> herror "src_of_instr: not an instr ZExt: " pr_instr i
  ;;

  let dst_of_instr_zext (i : instr) : value =
    match instr_opcode i with
    | LO.ZExt -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr ZExt: " pr_instr i
  ;;

  (* Call *)

  let num_args_of_instr_call (i : instr) : int =
    match instr_opcode i with
    | LO.Call -> LL.num_arg_operands (llvalue_of_instr i)
    | _ -> herror "num_args_of: not an instr Call: " pr_instr i
  ;;

  let callee_of_instr_call (i : instr) : func =
    match instr_opcode i with
    | LO.Call ->
      let num_args = num_args_of_instr_call i in
      mk_func (operand i num_args)
    | _ -> herror "callee_of: not an instr Call: " pr_instr i
  ;;

  let arg_of_instr_call (i : instr) (idx : int) : value =
    match instr_opcode i with
    | LO.Call ->
      if idx < num_args_of_instr_call i
      then operand i idx
      else herror "arg_of_instr_call: idx out of bound" pr_int idx
    | _ -> herror "arg_of: not an instr Call: " pr_instr i
  ;;

  let args_of_instr_call (i : instr) : values =
    match instr_opcode i with
    | LO.Call ->
      let args = ref [] in
      let num_args = num_args_of_instr_call i in
      for idx = 0 to num_args - 1 do
        args := !args @ [ operand i idx ]
      done;
      !args
    | _ -> herror "operand_args: not an instr Call: " pr_instr i
  ;;

  (* CallBr *)

  let num_args_of_instr_callbr (i : instr) : int =
    match instr_opcode i with
    | LO.CallBr -> LL.num_arg_operands (llvalue_of_instr i)
    | _ -> herror "num_args_of: not an instr CallBr: " pr_instr i
  ;;

  let arg_of_instr_callbr (i : instr) (idx : int) : value =
    match instr_opcode i with
    | LO.CallBr ->
      if idx < num_args_of_instr_call i
      then operand i idx
      else herror "arg_of_instr_call: idx out of bound" pr_int idx
    | _ -> herror "arg_of: not an instr Call: " pr_instr i
  ;;

  let args_of_instr_callbr (i : instr) : values =
    match instr_opcode i with
    | LO.CallBr ->
      let args = ref [] in
      let num_args = num_args_of_instr_call i in
      for idx = 0 to num_args - 1 do
        args := !args @ [ operand i idx ]
      done;
      !args
    | _ -> herror "operand_args: not an instr CallBr: " pr_instr i
  ;;

  let callee_of_instr_callbr (i : instr) : func =
    match instr_opcode i with
    | LO.CallBr ->
      let num_args = num_args_of_instr_callbr i in
      mk_func (operand i num_args)
    | _ -> herror "callee_of: not an instr CallBr: " pr_instr i
  ;;

  (* Invoke *)

  let num_args_of_instr_invoke (i : instr) : int =
    match instr_opcode i with
    | LO.Invoke -> LL.num_arg_operands (llvalue_of_instr i)
    | _ -> herror "num_args_of: not an instr Invoke: " pr_instr i
  ;;

  let callee_of_instr_invoke (i : instr) : func =
    match instr_opcode i with
    | LO.Invoke ->
      let num_args = num_args_of_instr_invoke i in
      mk_func (operand i (num_args + 2))
    | _ -> herror "callee_of: not an instr Invoke: " pr_instr i
  ;;

  let unwind_dest_of_instr_invoke (i : instr) : block =
    match instr_opcode i with
    | LO.Invoke -> LL.get_unwind_dest (llvalue_of_instr i)
    | _ -> herror "unwind_dest_of: not an instr Invoke: " pr_instr i
  ;;

  let normal_dest_of_instr_invoke (i : instr) : block =
    match instr_opcode i with
    | LO.Invoke -> LL.get_normal_dest (llvalue_of_instr i)
    | _ -> herror "normal_dest_of: not an instr Invoke: " pr_instr i
  ;;

  let arg_of_instr_invoke (i : instr) (idx : int) : value =
    match instr_opcode i with
    | LO.Invoke ->
      if idx < num_args_of_instr_call i
      then operand i idx
      else herror "arg_of_instr_invoke: idx out of bound" pr_int idx
    | _ -> herror "arg_of: not an instr Invoke: " pr_instr i
  ;;

  let args_of_instr_invoke (i : instr) : values =
    match instr_opcode i with
    | LO.Invoke ->
      let args = ref [] in
      let num_args = num_args_of_instr_invoke i in
      for idx = 0 to num_args - 1 do
        args := !args @ [ operand i idx ]
      done;
      !args
    | _ -> herror "operand_args: not an instr Invoke: " pr_instr i
  ;;

  (* Function application instructions are: Call, CallBr, Invoke *)

  let num_args_of_instr_func_app (i : instr) : int =
    match instr_opcode i with
    | LO.Call -> num_args_of_instr_call i
    | LO.CallBr -> num_args_of_instr_call i
    | LO.Invoke -> num_args_of_instr_invoke i
    | _ ->
      herror "num_args_of_instr_func_app: not a callable instr: " pr_instr i
  ;;

  let callee_of_instr_func_call (i : instr) : func =
    match instr_opcode i with
    | LO.Call -> callee_of_instr_call i
    | LO.CallBr -> callee_of_instr_callbr i
    | LO.Invoke -> callee_of_instr_invoke i
    | _ ->
      herror "callee_of_instr_func_call: not a callable instr: " pr_instr i
  ;;

  let arg_of_instr_func_app (i : instr) (idx : int) : value =
    match instr_opcode i with
    | LO.Call -> arg_of_instr_call i idx
    | LO.CallBr -> arg_of_instr_callbr i idx
    | LO.Invoke -> arg_of_instr_invoke i idx
    | _ -> herror "arg_of_instr_func_app: not a callable instr: " pr_instr i
  ;;

  let args_of_instr_func_app (i : instr) : values =
    match instr_opcode i with
    | LO.Call -> args_of_instr_call i
    | LO.CallBr -> args_of_instr_callbr i
    | LO.Invoke -> args_of_instr_invoke i
    | _ -> herror "args_of_instr_func_app: not a callable instr: " pr_instr i
  ;;

  let get_origin_src_of_memcpy (i : instr) : value =
    let callee = callee_of_instr_func_call i in
    if is_func_memcpy callee
    then operand (mk_instr (operand i 0)) 0
    else herror "get_origin_src_of_memcpy: not a memcopy Call: " pr_instr i
  ;;

  let get_origin_dst_of_memcpy (i : instr) : value =
    let callee = callee_of_instr_func_call i in
    if is_func_memcpy callee
    then operand (mk_instr (operand i 1)) 0
    else herror "get_origin_dst_of_memcpy: not a memcopy Call: " pr_instr i
  ;;

  (* Icmp *)

  let predicate_of_instr_icmp (i : instr) : LL.Icmp.t option =
    match instr_opcode i with
    | LO.ICmp -> LL.icmp_predicate (llvalue_of_instr i)
    | _ -> herror "predicate: not an instr Icmp: " pr_instr i
  ;;

  (* Fcmp *)

  let predicate_of_instr_fcmp (i : instr) : LL.Fcmp.t option =
    match instr_opcode i with
    | LO.FCmp -> LL.fcmp_predicate (llvalue_of_instr i)
    | _ -> herror "predicate: not an instr FCmp: " pr_instr i
  ;;

  (* Br *)

  let branch_of_instr_br (i : instr) =
    match instr_opcode i with
    | LO.Br | LO.IndirectBr -> LL.get_branch (llvalue_of_instr i)
    | _ -> herror "branch: not an instr Br: " pr_instr i
  ;;

  (* PHI Node *)

  let src_of_instr_phi (i : instr) : values =
    match instr_opcode i with
    | LO.PHI ->
      let operands = ref [] in
      for idx = 0 to num_operands i - 1 do
        operands := !operands @ [ operand i idx ]
      done;
      !operands
    | _ -> herror "operands: not an instr PHI: " pr_instr i
  ;;

  let src_and_origin_of_instr_phi (i : instr) : (value * block) list =
    match instr_opcode i with
    | LO.PHI -> LL.incoming (llvalue_of_instr i)
    | _ -> herror "operands: not an instr PHI: " pr_instr i
  ;;

  let dst_of_instr_phi (i : instr) : value =
    match instr_opcode i with
    | LO.PHI -> llvalue_of_instr i
    | _ -> herror "dst_of_instr: not an instr PHI: " pr_instr i
  ;;

  let is_phi_of_same_src_and_origin (i1 : instr) (i2 : instr) : bool =
    let src_origin1 = src_and_origin_of_instr_phi i1 in
    let src_origin2 = src_and_origin_of_instr_phi i2 in
    if List.length src_origin1 = List.length src_origin2
    then
      List.for_all2_exn
        ~f:(fun (v1, b1) (v2, b2) -> equal_value v1 v2 && equal_block b1 b2)
        src_origin1 src_origin2
    else false
  ;;
end

include InstrUtils

(*******************************************************************
 ** operations with path condition
 *******************************************************************)

module PathUtils = struct
  let mk_pred_true () = PBool true
  let mk_pred_false () = PBool false

  let mk_pred_icmp cmp (lhs : value) (rhs : value) : predicate =
    PIcmp (cmp, lhs, rhs)
  ;;

  let mk_pred_fcmp cmp (lhs : value) (rhs : value) : predicate =
    PFcmp (cmp, lhs, rhs)
  ;;

  let negate_icmp (cmp : LL.Icmp.t) : LL.Icmp.t =
    match cmp with
    | LL.Icmp.Eq -> LL.Icmp.Ne
    | LL.Icmp.Ne -> LL.Icmp.Eq
    | LL.Icmp.Ugt -> LL.Icmp.Ule
    | LL.Icmp.Uge -> LL.Icmp.Ult
    | LL.Icmp.Ult -> LL.Icmp.Uge
    | LL.Icmp.Ule -> LL.Icmp.Ugt
    | LL.Icmp.Sgt -> LL.Icmp.Sle
    | LL.Icmp.Sge -> LL.Icmp.Slt
    | LL.Icmp.Slt -> LL.Icmp.Sge
    | LL.Icmp.Sle -> LL.Icmp.Sgt
  ;;

  let mk_pred_neg (p : predicate) : predicate =
    match p with
    | PBool b -> PBool (not b)
    | PIcmp (cmp, lhs, rhs) -> PIcmp (negate_icmp cmp, lhs, rhs)
    | PNeg p -> p
    | _ -> PNeg p
  ;;

  let mk_pred_conj (ps : predicate list) : predicate =
    let rec flatten acc ps =
      match ps with
      | [] -> acc
      | PConj gs :: nps -> flatten (flatten acc gs) nps
      | p :: nps -> flatten (acc @ [ p ]) nps in
    if List.is_empty ps
    then mk_pred_false ()
    else (
      let nps = flatten [] ps in
      if List.exists ~f:is_pred_false nps
      then mk_pred_false ()
      else (
        let nps = List.stable_dedup (List.exclude ~f:is_pred_true nps) in
        match nps with
        | [] -> mk_pred_true ()
        | [ np ] -> np
        | _ -> PConj nps))
  ;;

  let mk_pred_disj (ps : predicate list) : predicate =
    let rec flatten acc ps =
      match ps with
      | [] -> acc
      | PDisj gs :: nps -> flatten (flatten acc gs) nps
      | p :: nps -> flatten (acc @ [ p ]) nps in
    if List.is_empty ps
    then mk_pred_true ()
    else (
      let nps = flatten [] ps in
      if List.exists ~f:is_pred_true nps
      then mk_pred_true ()
      else (
        let nps = List.stable_dedup (List.exclude ~f:is_pred_false nps) in
        match nps with
        | [] -> mk_pred_false ()
        | [ np ] -> np
        | _ -> PDisj nps))
  ;;

  let extract_icmp_predicate (cond : value) : predicate =
    match LL.icmp_predicate cond with
    | None -> herror "extract_icmp_predicate: not Icmp cond: " pr_value cond
    | Some cmp ->
      let lhs, rhs = LL.operand cond 0, LL.operand cond 1 in
      mk_pred_icmp cmp lhs rhs
  ;;

  let extract_fcmp_predicate (cond : value) : predicate =
    match LL.fcmp_predicate cond with
    | None -> herror "extract_fcmp_predicate: not Fcmp cond: " pr_value cond
    | Some cmp ->
      let lhs, rhs = LL.operand cond 0, LL.operand cond 1 in
      mk_pred_fcmp cmp lhs rhs
  ;;

  let extract_trunc_predicate (cond : value) : predicate =
    match LL.instr_opcode cond with
    | LO.Trunc ->
      (* FIXME: need better handling of Trunc, maybe by unfolding? *)
      mk_pred_true ()
    | _ -> herror "extract_trunc_predicate: not a Trunc cond: " pr_value cond
  ;;

  let extract_zext_predicate (cond : value) : predicate =
    match LL.instr_opcode cond with
    | LO.ZExt ->
      (* FIXME: need better handling of ZExt? *)
      mk_pred_true ()
    | _ -> herror "extract_zext_predicate: not a ZExt cond: " pr_value cond
  ;;

  let extract_br_cond_predicate (cond : value) : predicate =
    match LL.classify_value cond with
    | LV.Instruction _ ->
      (match LL.instr_opcode cond with
      | LO.ICmp -> extract_icmp_predicate cond
      | LO.FCmp -> extract_fcmp_predicate cond
      | LO.Trunc -> extract_trunc_predicate cond
      | LO.ZExt -> extract_zext_predicate cond
      | _ -> mk_pred_true ())
    | _ -> mk_pred_true ()
  ;;

end

include PathUtils


(*******************************************************************
 ** operations with blocks
 *******************************************************************)

(** Module contains utility functions for processing blocks *)

module BlockUtils = struct
  let block_name_full (blk : block) : string =
    let func = func_of_block blk in
    Func.func_name func ^ "_" ^ block_name blk
  ;;

  let index_of_block_name (blk : block) : int =
    let bname = block_name blk in
    if String.is_prefix bname ~prefix:"bb"
    then (
      let sindex = String.sub bname ~pos:2 ~len:(String.length bname - 2) in
      try Int.of_string sindex with _ -> -1)
    else -1
  ;;

  let index_of_instr_in_block (instr : instr) (blk : block) : int option =
    let rec traverse acc ins : int option =
      if equal_instr ins instr
      then Some acc
      else (
        match instr_succ ins with
        | None -> None
        | Some ins' -> traverse (acc + 1) ins') in
    match instr_begin blk with
    | None -> None
    | Some ins -> traverse 0 ins
  ;;

  let compare_block_by_name blk1 blk2 : int =
    let idx1, idx2 = index_of_block_name blk1, index_of_block_name blk2 in
    if idx1 < idx2 then -1 else if idx1 > idx2 then 1 else 0
  ;;

  let is_entry_block (blk : block) func : bool =
    equal_block blk (LL.entry_block func)
  ;;

  let last_instr_of_block (blk : block) : instr option =
    match LL.instr_end blk with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_instr v)
  ;;

  let first_instr_of_block (blk : block) : instr option =
    match LL.instr_begin blk with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_instr v)
  ;;

  let is_first_instr_of_block (instr : instr) : bool =
    let blk = block_of_instr instr in
    match first_instr_of_block blk with
    | None -> false
    | Some i -> equal_instr i instr
  ;;

  let is_entry_block_of_function (blk : block) : bool =
    match LL.block_pred blk with
    | LL.At_start _ -> true
    | LL.After _ -> false
  ;;

  let block_of_prec_block (pblk : prec_block) : block = pblk.pblk_block
  let block_of_succ_block (sblk : succ_block) : block = sblk.sblk_block

  let get_preceding_blocks (prog : program) (blk : block) : prec_blocks =
    let compute_blocks (blk : block) : prec_blocks =
      let func = func_of_block blk in
      fold_left_blocks
        ~f:(fun acc1 blk1 ->
          fold_left_instrs
            ~f:(fun acc2 instr ->
              match instr_opcode instr with
              | LO.IndirectBr | LO.Br ->
                (match get_branch instr with
                | None -> acc2
                | Some (`Unconditional blk2) ->
                  if equal_block blk blk2
                  then acc2 @ [ mk_prec_block blk1 (mk_pred_true ()) ]
                  else acc2
                | Some (`Conditional (cond, blk21, blk22)) ->
                  let pred = extract_br_cond_predicate cond in
                  if equal_block blk blk21
                  then acc2 @ [ mk_prec_block blk1 pred ]
                  else if equal_block blk blk22
                  then acc2 @ [ mk_prec_block blk1 (mk_pred_neg pred) ]
                  else acc2)
              | LO.Switch ->
                let pblks = ref [] in
                for i = 0 to (num_operands instr / 2) - 1 do
                  let blk2 = LL.block_of_value (operand instr ((i * 2) + 1)) in
                  if equal_block blk blk2
                  then pblks := [ mk_prec_block blk (mk_pred_true ()) ]
                done;
                acc2 @ !pblks
              | LO.Invoke ->
                let blk21 = normal_dest_of_instr_invoke instr in
                let blk22 = unwind_dest_of_instr_invoke instr in
                if equal_block blk blk21
                then acc2 @ [ mk_prec_block blk1 (mk_pred_true ()) ]
                else if equal_block blk blk22
                then acc2 @ [ mk_prec_block blk1 (mk_pred_true ()) ]
                else acc2
              | _ -> acc2)
            ~init:acc1 blk1)
        ~init:[] func in
    Hashtbl.find_or_compute prog.prog_block_data.pbd_preceding_blocks
      ~f:(fun () -> compute_blocks blk)
      ~key:blk
  ;;

  let get_succeeding_blocks (prog : program) (blk : block) : succ_blocks =
    let compute_blocks (blk : block) : succ_blocks =
      fold_left_instrs
        ~f:(fun acc instr ->
          match instr_opcode instr with
          | LO.IndirectBr | LO.Br ->
            let sblks =
              match get_branch instr with
              | None -> []
              | Some (`Unconditional b) ->
                [ mk_succ_block b (mk_pred_true ()) ]
              | Some (`Conditional (cond, b1, b2)) ->
                let pred = extract_br_cond_predicate cond in
                let sblk1 = mk_succ_block b1 pred in
                let sblk2 = mk_succ_block b2 (mk_pred_neg pred) in
                [ sblk1; sblk2 ] in
            acc @ sblks
          | LO.Switch ->
            let sblks = ref [] in
            for i = 0 to (num_operands instr / 2) - 1 do
              let blk = LL.block_of_value (operand instr ((i * 2) + 1)) in
              let sblk = mk_succ_block blk (mk_pred_true ()) in
              sblks := !sblks @ [ sblk ]
            done;
            acc @ !sblks
          | LO.Invoke ->
            let blk1 = normal_dest_of_instr_invoke instr in
            let blk2 = unwind_dest_of_instr_invoke instr in
            let sblk1 = mk_succ_block blk1 (mk_pred_true ()) in
            let sblk2 = mk_succ_block blk2 (mk_pred_true ()) in
            acc @ [ sblk1; sblk2 ]
          | _ -> acc)
        ~init:[] blk in
    Hashtbl.find_or_compute prog.prog_block_data.pbd_succeeding_blocks
      ~f:(fun () -> compute_blocks blk)
      ~key:blk
  ;;

  let has_unique_path_between_blocks prog (src : block) (dst : block) : bool =
    let rec check_path blk =
      if equal_block blk dst
      then true
      else (
        match get_succeeding_blocks prog blk with
        | [ sblk ] -> check_path sblk.sblk_block
        | _ -> false) in
    check_path src
  ;;

  let get_succeeding_only_blocks (prog : program) (blk : block) : blocks =
    blk |> get_succeeding_blocks prog |> List.map ~f:(fun sb -> sb.sblk_block)
  ;;

  let get_pathcond_between_blocks prog (src : block) (dst : block)
      : predicate option
    =
    let sblks = get_succeeding_blocks prog src in
    let sblks =
      List.filter ~f:(fun sblk -> equal_block sblk.sblk_block dst) sblks in
    match sblks with
    | [] -> None
    | sblk :: _ -> Some sblk.sblk_pathcond
  ;;
end

include BlockUtils

(*******************************************************************
 ** operations with functions and parameters
 *******************************************************************)

(** Module contains utility functions for processing functions *)
module FuncUtils = struct
  (* formal parameters *)
  let formal_params_of_func (f : func) : param list =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function -> fold_left_params ~f:(fun acc p -> acc @ [ p ]) ~init:[] f
    | _ -> herror "formal_params_of_func: not an actual function: " pr_value v
  ;;

  let entry_block (f : func) : block = LL.entry_block (llvalue_of_func f)

  let blocks_of_func (f : func) : blocks =
    fold_left_blocks ~f:(fun acc blk -> acc @ [ blk ]) ~init:[] f
  ;;

  let delete_function (f : func) : unit =
    LL.delete_function (llvalue_of_func f)
  ;;

  let first_block_of_func (f : func) : block option =
    match LL.block_begin (llvalue_of_func f) with
    | LL.At_end _ -> None
    | LL.Before blk -> Some blk
  ;;

  let last_block_of_func (f : func) : block option =
    match LL.block_end (llvalue_of_func f) with
    | LL.At_start _ -> None
    | LL.After blk -> Some blk
  ;;

  let is_first_block_of_func (blk : block) : bool =
    let func = func_of_block blk in
    match first_block_of_func func with
    | None -> false
    | Some b -> equal_block b blk
  ;;

  let first_instr_of_func (f : func) : instr option =
    match first_block_of_func f with
    | None -> None
    | Some b -> first_instr_of_block b
  ;;

  let get_func_callees (prog : program) (f : func) : funcs =
    match Hashtbl.find prog.prog_func_data.pfd_callees f with
    | None -> []
    | Some callables ->
      List.fold_right
        ~f:(fun cl acc ->
          match cl with
          | ClFunc f -> f :: acc
          | _ -> acc)
        ~init:[] callables
  ;;

  let get_func_ptr_callees (prog : program) (f : func) : values =
    match Hashtbl.find prog.prog_func_data.pfd_callees f with
    | None -> []
    | Some callables ->
      List.fold_right
        ~f:(fun cl acc ->
          match cl with
          | ClFPtr vfp -> vfp :: acc
          | _ -> acc)
        ~init:[] callables
  ;;

  let get_func_callers (prog : program) (f : func) : funcs =
    match Hashtbl.find prog.prog_func_data.pfd_callers f with
    | None -> []
    | Some fns -> fns
  ;;

  let get_func_used_globals (prog : program) (f : func) : globals =
    if is_user_func f || is_init_func f
    then (
      match Hashtbl.find prog.prog_func_data.pfd_used_globals f with
      | None -> herror "get_func_used_globals: no infor of: " func_name f
      | Some gs -> gs)
    else []
  ;;

  let has_call_to_user_funcs prog (f : func) : bool =
    let callees = get_func_callees prog f in
    List.exists ~f:(fun f -> is_user_func f || is_func_pointer f) callees
  ;;

  let local_vars_of_func (f : func) : instr list =
    fold_left_blocks
      ~f:(fun acc1 blk ->
        let allocas =
          fold_left_instrs
            ~f:(fun acc2 instr ->
              match instr_opcode instr with
              | LO.Alloca -> acc2 @ [ instr ]
              | _ -> acc2)
            ~init:[] blk in
        acc1 @ allocas)
      ~init:[] f
  ;;
end

include FuncUtils


(*******************************************************************
 ** substitution
 *******************************************************************)

(** Module contains function to process substitution *)
module Substitution = struct
  type substv = (value * value) list (* old / new values*)

  type substve = (value * expr) list (* old / new values*)

  type subste = (expr * expr) list (* old / new exprs*)

  let pr_substv (sst : substv) : string =
    pr_list ~f:(pr_pair ~f1:pr_value ~f2:pr_value) sst
  ;;

  let pr_subste (sst : subste) : string =
    pr_list ~f:(pr_pair ~f1:pr_expr ~f2:pr_expr) sst
  ;;

  let init_substv () : substv = []
  let init_subste () : subste = []

  let extend_substv (sst : substv) oldv newv : substv =
    if List.exists ~f:(fun (a, _) -> equal_value oldv a) sst
    then sst
    else (oldv, newv) :: sst
  ;;

  let extend_subste (sst : subste) olde newe : subste =
    if List.exists ~f:(fun (a, _) -> equal_expr olde a) sst
    then sst
    else (olde, newe) :: sst
  ;;

  let extend_substve (sst : substve) oldv newe : substve =
    if List.exists ~f:(fun (a, _) -> equal_value oldv a) sst
    then sst
    else (oldv, newe) :: sst
  ;;

  let mk_substv ~(oldvs : value list) ~(newvs : value list) : substv =
    try
      List.fold2_exn
        ~f:(fun acc ov nv -> extend_substv acc ov nv)
        ~init:[] oldvs newvs
    with _ -> []
  ;;

  let mk_subste ~(oldes : expr list) ~(newes : expr list) : subste =
    try
      List.fold2_exn
        ~f:(fun acc oe ne -> extend_subste acc oe ne)
        ~init:[] oldes newes
    with _ -> []
  ;;

  let mk_substve ~(oldvs : value list) ~(newes : expr list) : substve =
    try
      List.fold2_exn
        ~f:(fun acc ov ne -> extend_substve acc ov ne)
        ~init:[] oldvs newes
    with _ -> []
  ;;

  let subst_value (sst : substv) (v : value) : value =
    let res = List.find ~f:(fun (a, b) -> equal_value a v) sst in
    match res with
    | Some (_, b) -> b
    | None -> v
  ;;

  let subst_value_expr (sst : substve) (v : value) : expr =
    let res = List.find ~f:(fun (a, b) -> equal_value a v) sst in
    match res with
    | Some (_, b) -> b
    | None -> Var v
  ;;

  let subst_values (sst : substv) (vs : value list) : value list =
    List.fold_left ~f:(fun acc v -> acc @ [ subst_value sst v ]) ~init:[] vs
  ;;

  let rec subst_expr
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (e : expr)
      : expr
    =
    (* first substitute expression *)
    let res = List.find ~f:(fun (a, b) -> equal_expr a e) sste in
    match res with
    | Some (_, b) -> b
    | None ->
      if (* if not successful, substitute value or value/expr *)
         List.not_empty sstv
      then (
        match e with
        | Undef _ | Int64 _ | Float _ | String _ -> e
        | Var v -> Var (subst_value sstv v)
        | OldE e -> OldE (subst_expr ~sstv ~sstve ~sste e)
        | Deref e -> Deref (subst_expr ~sstv ~sstve ~sste e)
        | ElemPtr (root, rtyp, idxs) ->
          let nroot = subst_expr ~sstv ~sstve ~sste root in
          let nidxs = List.map ~f:(subst_expr ~sstv ~sstve ~sste) idxs in
          ElemPtr (nroot, rtyp, nidxs)
        | Malloc e -> Malloc (subst_expr ~sstv ~sstve ~sste e)
        | FuncRes _ -> e
        | Exn e -> Exn (subst_expr ~sstv ~sstve ~sste e))
      else (
        match e with
        | Undef _ | Int64 _ | Float _ | String _ -> e
        | Var v -> subst_value_expr sstve v
        | OldE _ -> e
        | Deref e -> Deref (subst_expr ~sstv ~sstve ~sste e)
        | ElemPtr (root, rtyp, idxs) ->
          let nroot = subst_expr ~sstv ~sstve ~sste root in
          let nidxs = List.map ~f:(subst_expr ~sstv ~sstve ~sste) idxs in
          ElemPtr (nroot, rtyp, nidxs)
        | Malloc e -> Malloc (subst_expr ~sstv ~sstve ~sste e)
        | FuncRes _ -> e
        | Exn _ -> e)
  ;;
end

include Substitution

(*******************************************************************
 ** Other
 *******************************************************************)

let print_pointer_stats (modul : LL.llmodule) : unit =
  let num_struct_vars = ref 0 in
  let num_array_vars = ref 0 in
  let num_pointer_vars = ref 0 in
  let num_instrs = ref 0 in
  let num_blks = ref 0 in
  let num_user_funcs = ref 0 in
  let num_func_calls = ref 0 in
  let update_stats_of_llvalue v =
    let t = LL.type_of v in
    let users = get_users v in
    let _ = if is_type_array t then incr num_array_vars in
    let _ = if is_type_struct t then incr num_struct_vars in
    if is_type_pointer t
    then (
      let elem_typ = LL.element_type t in
      let _ =
        if is_type_struct elem_typ
        then incr num_struct_vars
        else if is_type_array elem_typ
        then incr num_array_vars
        else if List.exists ~f:is_llvalue_instr_gep users
        then incr num_array_vars
        else () in
      incr num_pointer_vars) in
  let visit_global g =
    let t = LL.element_type (type_of_global g) in
    let users = get_users (llvalue_of_global g) in
    let _ = if is_type_array t then incr num_array_vars in
    let _ = if is_type_struct t then incr num_struct_vars in
    if is_type_pointer t
    then (
      let elem_typ = LL.element_type t in
      let _ =
        if is_type_struct elem_typ
        then incr num_struct_vars
        else if is_type_array elem_typ
        then incr num_array_vars
        else if List.exists ~f:is_llvalue_instr_gep users
        then incr num_array_vars
        else () in
      incr num_pointer_vars) in
  let visit_param p =
    let vp = llvalue_of_param p in
    update_stats_of_llvalue vp in
  let visit_instr i =
    let vi = llvalue_of_instr i in
    let _ = update_stats_of_llvalue vi in
    let _ = if is_instr_call_invoke i then incr num_func_calls in
    incr num_instrs in
  let visit_block blk =
    let _ = incr num_blks in
    None in
  let visit_func f =
    let _ = incr num_user_funcs in
    None in
  let _ =
    iter_struct_module ~fglobal:(Some visit_global) ~ffunc:(Some visit_func)
      ~fparam:(Some visit_param) ~fblock:(Some visit_block)
      ~finstr:(Some visit_instr) modul in
  let stats =
    "\nPointer Statistics:\n"
    ^ sprintf "  #User funcs: %d\n" !num_user_funcs
    ^ sprintf "  #Blocks: %d\n" !num_blks
    ^ sprintf "  #Instrs: %d\n" !num_instrs
    ^ sprintf "  #Func calls: %d\n" !num_func_calls
    ^ sprintf "  #Pointer Vars: %d\n" !num_pointer_vars
    ^ sprintf "  #Struct Vars: %d\n" !num_struct_vars
    ^ sprintf "  #Array Vars: %d\n" !num_array_vars in
  print ~autoformat:false ~always:true stats
;;
