(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llast
open Source
module LD = Llvm_debuginfo

(*******************************************************************
 * Iteration over LLVM data structures
 *******************************************************************)

module Iter = struct
  let iter_instrs ~(f : instr -> unit) (blk : block) : unit =
    let ff v = f (mk_instr v) in
    LL.iter_instrs ff blk
  ;;

  let iter_blocks ~(f : block -> unit) (fn : func) : unit =
    LL.iter_blocks f (llvalue_of_func fn)
  ;;

  let iter_params ~(f : param -> unit) (fn : func) : unit =
    let ff v = f (mk_param v) in
    LL.iter_params ff (llvalue_of_func fn)
  ;;

  let iter_globals ~(f : global -> unit) (m : bitcode_module) : unit =
    let ff v = f (mk_global v) in
    LL.iter_globals ff m
  ;;

  let iter_functions ~(f : func -> unit) (m : bitcode_module) : unit =
    let ff v = f (mk_func v) in
    LL.iter_functions ff m
  ;;
end

include Iter

(*******************************************************************
 * Mapping over LLVM data structures
 *******************************************************************)

module Map = struct
  let map_instrs ~(f : instr -> 'a) (blk : block) : 'a list =
    let ff acc v = acc @ [ f (mk_instr v) ] in
    LL.fold_left_instrs ff [] blk
  ;;

  let map_blocks ~(f : block -> 'a) (func : func) : 'a list =
    let ff acc b = acc @ [ f b ] in
    LL.fold_left_blocks ff [] (llvalue_of_func func)
  ;;

  let map_params ~(f : param -> 'a) (func : func) : 'a list =
    let ff acc v = acc @ [ f (mk_param v) ] in
    LL.fold_left_params ff [] (llvalue_of_func func)
  ;;

  let map_globals ~(f : global -> 'a) (m : bitcode_module) : 'a list =
    let ff acc v = acc @ [ f (mk_global v) ] in
    LL.fold_left_globals ff [] m
  ;;

  let map_functions ~(f : func -> 'a) (m : bitcode_module) : 'a list =
    let ff acc v = acc @ [ f (mk_func v) ] in
    LL.fold_left_functions ff [] m
  ;;
end

include Map

(*******************************************************************
 * Checking existence over LLVM data structures
 *******************************************************************)

module Exists = struct
  let exists_use ~(f : use -> bool) (v : value) : bool =
    let rec check_use (u : use) : bool =
      if f u
      then true
      else (
        match use_succ u with
        | None -> false
        | Some u' -> check_use u') in
    match use_begin v with
    | None -> false
    | Some u -> check_use u
  ;;

  let exists_instr ~(f : instr -> bool) (blk : block) : bool =
    let rec check_instr (ins : instr) : bool =
      if f ins
      then true
      else (
        match instr_succ ins with
        | None -> false
        | Some ins' -> check_instr ins') in
    match first_instr_of_block blk with
    | None -> false
    | Some ins -> check_instr ins
  ;;

  let exists_block ~(f : block -> bool) (fn : func) : bool =
    let rec check_block (blk : block) : bool =
      if f blk
      then true
      else (
        match block_succ blk with
        | None -> false
        | Some blk' -> check_block blk') in
    match first_block_of_func fn with
    | None -> false
    | Some blk -> check_block blk
  ;;

  let exists_param ~(f : param -> bool) (fn : func) : bool =
    let rec check_param (p : param) : bool =
      if f p
      then true
      else (
        match param_succ p with
        | None -> false
        | Some p' -> check_param p') in
    match first_param_of_func fn with
    | None -> false
    | Some p -> check_param p
  ;;

  let exists_global ~(f : global -> bool) (m : bitcode_module) : bool =
    let rec check_global (g : global) : bool =
      if f g
      then true
      else (
        match global_succ g with
        | None -> false
        | Some g' -> check_global g') in
    match first_global_of_module m with
    | None -> false
    | Some g -> check_global g
  ;;

  let exists_function ~(f : func -> bool) (m : bitcode_module) : bool =
    let rec check_func (fn : func) : bool =
      if f fn
      then true
      else (
        match func_succ fn with
        | None -> false
        | Some fn' -> check_func fn') in
    match first_func_of_module m with
    | None -> false
    | Some fn -> check_func fn
  ;;
end

include Exists

(*******************************************************************
 * Checking for-all over LLVM data structures
 *******************************************************************)

module ForAll = struct
  let for_all_use ~(f : use -> bool) (v : value) : bool =
    let rec check_use (u : use) : bool =
      if not (f u)
      then false
      else (
        match use_succ u with
        | None -> true
        | Some u' -> check_use u') in
    match use_begin v with
    | None -> true
    | Some u -> check_use u
  ;;

  let for_all_instr ~(f : instr -> bool) (blk : block) : bool =
    let rec check_instr (ins : instr) : bool =
      if not (f ins)
      then false
      else (
        match instr_succ ins with
        | None -> true
        | Some ins' -> check_instr ins') in
    match first_instr_of_block blk with
    | None -> true
    | Some ins -> check_instr ins
  ;;

  let for_all_block ~(f : block -> bool) (fn : func) : bool =
    let rec check_block (blk : block) : bool =
      if not (f blk)
      then false
      else (
        match block_succ blk with
        | None -> true
        | Some blk' -> check_block blk') in
    match first_block_of_func fn with
    | None -> true
    | Some blk -> check_block blk
  ;;

  let for_all_param ~(f : param -> bool) (fn : func) : bool =
    let rec check_param (p : param) : bool =
      if not (f p)
      then false
      else (
        match param_succ p with
        | None -> true
        | Some p' -> check_param p') in
    match first_param_of_func fn with
    | None -> true
    | Some p -> check_param p
  ;;

  let for_all_global ~(f : global -> bool) (m : bitcode_module) : bool =
    let rec check_global (g : global) : bool =
      if not (f g)
      then false
      else (
        match global_succ g with
        | None -> true
        | Some g' -> check_global g') in
    match first_global_of_module m with
    | None -> true
    | Some g -> check_global g
  ;;

  let for_all_function ~(f : func -> bool) (m : bitcode_module) : bool =
    let rec check_func (fn : func) : bool =
      if not (f fn)
      then false
      else (
        match func_succ fn with
        | None -> true
        | Some fn' -> check_func fn') in
    match first_func_of_module m with
    | None -> true
    | Some fn -> check_func fn
  ;;
end

include ForAll

(*******************************************************************
 * Folding over LLVM data structures
 *******************************************************************)

module Fold = struct
  let fold_left_instrs ~(f : 'a -> instr -> 'a) ~(init : 'a) (blk : block) : 'a
    =
    let ff acc v = f acc (mk_instr v) in
    LL.fold_left_instrs ff init blk
  ;;

  let fold_left_blocks ~(f : 'a -> block -> 'a) ~(init : 'a) (func : func) : 'a
    =
    LL.fold_left_blocks f init (llvalue_of_func func)
  ;;

  let fold_left_params ~(f : 'a -> param -> 'a) ~(init : 'a) (func : func) : 'a
    =
    let ff acc v = f acc (mk_param v) in
    LL.fold_left_params ff init (llvalue_of_func func)
  ;;

  let fold_left_globals
      ~(f : 'a -> global -> 'a)
      ~(init : 'a)
      (m : bitcode_module)
      : 'a
    =
    let ff acc v = f acc (mk_global v) in
    LL.fold_left_globals ff init m
  ;;

  let fold_left_functions
      ~(f : 'a -> func -> 'a)
      ~(init : 'a)
      (m : bitcode_module)
      : 'a
    =
    let ff acc v = f acc (mk_func v) in
    LL.fold_left_functions ff init m
  ;;
end

include Fold

(*******************************************************************
 * Visit LLVM data structures
 *******************************************************************)

module Visit = struct
  let visit_block
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (blk : block)
      : unit
    =
    let visit () =
      match finstr with
      | None -> ()
      | Some f -> iter_instrs ~f blk in
    match fblock with
    | None -> visit ()
    | Some f ->
      (match f blk with
      | None -> visit ()
      | Some res -> res)
  ;;

  let visit_func
      ?(ffunc : (func -> unit option) option = None)
      ?(fparam : (param -> unit) option = None)
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (fn : func)
      : unit
    =
    let visit () =
      let _ =
        match fparam with
        | None -> ()
        | Some f -> iter_params ~f fn in
      iter_blocks ~f:(visit_block ~fblock ~finstr) fn in
    match ffunc with
    | None -> visit ()
    | Some f ->
      (match f fn with
      | None -> visit ()
      | Some res -> res)
  ;;

  let visit_module
      ?(fglobal : (global -> unit) option = None)
      ?(ffunc : (func -> unit option) option = None)
      ?(fparam : (param -> unit) option = None)
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (m : bitcode_module)
      : unit
    =
    let _ =
      match fglobal with
      | None -> ()
      | Some f -> iter_globals ~f m in
    LL.iter_functions
      (fun f -> visit_func ~ffunc ~fparam ~fblock ~finstr (mk_func f))
      m
  ;;

  let visit_program
      ?(fglobal : (global -> unit) option = None)
      ?(ffunc : (func -> unit option) option = None)
      ?(fparam : (param -> unit) option = None)
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (prog : program)
      : unit
    =
    let _ =
      match fglobal with
      | None -> ()
      | Some f -> List.iter ~f prog.prog_globals in
    List.iter
      ~f:(visit_func ~ffunc ~fparam ~fblock ~finstr)
      (prog.prog_init_funcs @ prog.prog_user_funcs)
  ;;
end

include Visit

(*******************************************************************
 * Visit and fold LLVM data structures
 *******************************************************************)

module VisitFold = struct
  let visit_fold_block
      ?(fblock : ('a -> block -> 'a option) option = None)
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (blk : block)
      : 'a
    =
    let visit_fold () =
      match finstr with
      | None -> acc
      | Some f -> fold_left_instrs ~f ~init:acc blk in
    match fblock with
    | None -> visit_fold ()
    | Some f ->
      (match f acc blk with
      | None -> visit_fold ()
      | Some res -> res)
  ;;

  let visit_fold_func
      ?(ffunc : ('a -> func -> 'a option) option = None)
      ?(fparam : ('a -> param -> 'a) option = None)
      ?(fblock : ('a -> block -> 'a option) option = None)
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (fn : func)
      : 'a
    =
    let visit_fold () =
      let res =
        match fparam with
        | None -> acc
        | Some f -> fold_left_params ~f ~init:acc fn in
      fold_left_blocks ~f:(visit_fold_block ~fblock ~finstr) ~init:res fn in
    match ffunc with
    | None -> visit_fold ()
    | Some f ->
      (match f acc fn with
      | None -> visit_fold ()
      | Some res -> res)
  ;;

  let visit_fold_module
      ?(fglobal : ('a -> global -> 'a) option = None)
      ?(ffunc : ('a -> func -> 'a option) option = None)
      ?(fparam : ('a -> param -> 'a) option = None)
      ?(fblock : ('a -> block -> 'a option) option = None)
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (m : bitcode_module)
      : 'a
    =
    let res =
      match fglobal with
      | None -> acc
      | Some f -> fold_left_globals ~f ~init:acc m in
    fold_left_functions
      ~f:(fun acc' fn ->
        visit_fold_func ~ffunc ~fparam ~fblock ~finstr acc' fn)
      ~init:res m
  ;;

  let visit_fold_program
      ?(fglobal : ('a -> global -> 'a) option = None)
      ?(ffunc : ('a -> func -> 'a option) option = None)
      ?(fparam : ('a -> param -> 'a) option = None)
      ?(fblock : ('a -> block -> 'a option) option = None)
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (prog : program)
      : 'a
    =
    let res =
      match fglobal with
      | None -> acc
      | Some f -> List.fold_left ~f ~init:acc prog.prog_globals in
    List.fold_left
      ~f:(visit_fold_func ~ffunc ~fparam ~fblock ~finstr)
      ~init:res
      (prog.prog_init_funcs @ prog.prog_user_funcs)
  ;;
end

include VisitFold

(*******************************************************************
 * Visit and check existence over LLVM data structures
 *******************************************************************)

module VisitExists = struct
  let visit_exists_block
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (blk : block)
      : 'a
    =
    let visit_exists () =
      match finstr with
      | None -> false
      | Some f -> exists_instr ~f blk in
    match fblock with
    | None -> visit_exists ()
    | Some f ->
      (match f blk with
      | None -> visit_exists ()
      | Some res -> res)
  ;;

  let visit_exists_func
      ?(ffunc : (func -> bool option) option = None)
      ?(fparam : (param -> bool) option = None)
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (fn : func)
      : bool
    =
    let visit_exists () =
      let res =
        match fparam with
        | None -> false
        | Some f -> exists_param ~f fn in
      if res
      then true
      else exists_block ~f:(visit_exists_block ~fblock ~finstr) fn in
    match ffunc with
    | None -> visit_exists ()
    | Some f ->
      (match f fn with
      | None -> visit_exists ()
      | Some res -> res)
  ;;

  let visit_exists_module
      ?(fglobal : (global -> bool) option = None)
      ?(ffunc : (func -> bool option) option = None)
      ?(fparam : (param -> bool) option = None)
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (m : bitcode_module)
      : bool
    =
    let res =
      match fglobal with
      | None -> false
      | Some f -> exists_global ~f m in
    if res
    then true
    else
      exists_function ~f:(visit_exists_func ~ffunc ~fparam ~fblock ~finstr) m
  ;;

  let visit_exists_program
      ?(fglobal : (global -> bool) option = None)
      ?(ffunc : (func -> bool option) option = None)
      ?(fparam : (param -> bool) option = None)
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (prog : program)
      : bool
    =
    let res =
      match fglobal with
      | None -> false
      | Some f -> List.exists ~f prog.prog_globals in
    if res
    then true
    else
      List.exists
        ~f:(visit_exists_func ~ffunc ~fparam ~fblock ~finstr)
        (prog.prog_init_funcs @ prog.prog_user_funcs)
  ;;
end

include VisitExists

(*******************************************************************
 * Visit and check for-all over LLVM data structures
 *******************************************************************)

module VisitForAll = struct
  let visit_for_all_block
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (blk : block)
      : 'a
    =
    let visit_forall () =
      match finstr with
      | None -> true
      | Some f -> for_all_instr ~f blk in
    match fblock with
    | None -> visit_forall ()
    | Some f ->
      (match f blk with
      | None -> visit_forall ()
      | Some res -> res)
  ;;

  let visit_for_all_func
      ?(ffunc : (func -> bool option) option = None)
      ?(fparam : (param -> bool) option = None)
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (fn : func)
      : bool
    =
    let visit_forall () =
      let res =
        match fparam with
        | None -> true
        | Some f -> for_all_param ~f fn in
      if res
      then true
      else for_all_block ~f:(visit_for_all_block ~fblock ~finstr) fn in
    match ffunc with
    | None -> visit_forall ()
    | Some f ->
      (match f fn with
      | None -> visit_forall ()
      | Some res -> res)
  ;;

  let visit_for_all_module
      ?(fglobal : (global -> bool) option = None)
      ?(ffunc : (func -> bool option) option = None)
      ?(fparam : (param -> bool) option = None)
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (m : bitcode_module)
      : bool
    =
    let res =
      match fglobal with
      | None -> true
      | Some f -> for_all_global ~f m in
    if res
    then true
    else
      for_all_function ~f:(visit_for_all_func ~ffunc ~fparam ~fblock ~finstr) m
  ;;

  let visit_for_all_program
      ?(fglobal : (global -> bool) option = None)
      ?(ffunc : (func -> bool option) option = None)
      ?(fparam : (param -> bool) option = None)
      ?(fblock : (block -> bool option) option = None)
      ?(finstr : (instr -> bool) option = None)
      (prog : program)
      : bool
    =
    let res =
      match fglobal with
      | None -> true
      | Some f -> List.for_all ~f prog.prog_globals in
    if res
    then true
    else
      List.for_all
        ~f:(visit_for_all_func ~ffunc ~fparam ~fblock ~finstr)
        (prog.prog_init_funcs @ prog.prog_user_funcs)
  ;;
end

include VisitForAll

(*******************************************************************
 * Utilities functions for data types
 *******************************************************************)

module Type = struct
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
    let process_global g =
      collect_struct_type (LL.type_of (llvalue_of_global g)) in
    let process_instr i =
      collect_struct_type (LL.type_of (llvalue_of_instr i)) in
    let _ =
      visit_module ~finstr:(Some process_instr) ~fglobal:(Some process_global)
        m in
    Set.to_list !all_stypes
  ;;
end

include Type

(*******************************************************************
 ** operations with value
 *******************************************************************)

(** Module contains utility functions to process LLVM Values *)

module Value = struct
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

include Value

(*******************************************************************
 ** operations with use
 *******************************************************************)

(** Module contains utility functions to process LLVM Use *)

module Use = struct
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

include Use

(*******************************************************************
 ** operations with globals
 *******************************************************************)

(** Module contains utility functions to process global variables *)

module Global = struct
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

include Global

(*******************************************************************
 ** Utility functions to process instructions
 *******************************************************************)

(** Module contains utility functions to process instructions *)

module Instr = struct
  (* General instruction utilities *)

  let is_instr_same_block (i1 : instr) (i2 : instr) : bool =
    let blk1 = block_of_instr i1 in
    let blk2 = block_of_instr i2 in
    equal_block blk1 blk2
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

include Instr

(*******************************************************************
 ** operations with blocks
 *******************************************************************)

(** Module contains utility functions for processing blocks *)

module Block = struct
  let pr_block (blk : block) : string =
    let blkname = block_name blk in
    let sinstrs =
      blk
      |> map_instrs ~f:(String.hindent 2 pr_instr)
      |> String.concat ~sep:"\n" in
    (" " ^ blkname ^ ":\n")
    ^ String.replace_if_empty sinstrs ~replacer:"{Empty block}"
  ;;

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
end

include Block

(*******************************************************************
 ** operations with path condition
 *******************************************************************)

module Path = struct
  (*** Printing ***)

  let pr_icmp (cmp : LL.Icmp.t) : string =
    match cmp with
    | LL.Icmp.Eq -> "="
    | LL.Icmp.Ne -> "!="
    | LL.Icmp.Ugt -> ">"
    | LL.Icmp.Uge -> ">="
    | LL.Icmp.Ult -> "<"
    | LL.Icmp.Ule -> "<="
    | LL.Icmp.Sgt -> ">"
    | LL.Icmp.Sge -> ">="
    | LL.Icmp.Slt -> "<"
    | LL.Icmp.Sle -> "<="
  ;;

  let pr_fcmp (cmp : LL.Fcmp.t) : string =
    match cmp with
    | LL.Fcmp.False -> "False"
    | LL.Fcmp.Oeq -> "=="
    | LL.Fcmp.Ogt -> ">"
    | LL.Fcmp.Oge -> ">="
    | LL.Fcmp.Olt -> "<"
    | LL.Fcmp.Ole -> "<="
    | LL.Fcmp.One -> "!="
    | LL.Fcmp.Ord -> "NaN"
    | LL.Fcmp.Uno -> "NaN"
    | LL.Fcmp.Ueq -> "=="
    | LL.Fcmp.Ugt -> ">"
    | LL.Fcmp.Uge -> ">="
    | LL.Fcmp.Ult -> "<"
    | LL.Fcmp.Ule -> "<="
    | LL.Fcmp.Une -> "!="
    | LL.Fcmp.True -> "True"
  ;;

  let rec pr_predicate (p : predicate) : string =
    match p with
    | PBool b -> pr_bool b
    | PIcmp (cmp, lhs, rhs) -> pr_value lhs ^ pr_icmp cmp ^ pr_value rhs
    | PFcmp (cmp, lhs, rhs) -> pr_value lhs ^ pr_fcmp cmp ^ pr_value rhs
    | PNeg p -> "!" ^ pr_predicate p
    | PConj ps -> pr_list_plain ~sep:" & " ~f:pr_predicate ps
    | PDisj ps -> pr_list_plain ~sep:" | " ~f:pr_predicate ps
  ;;

  let pr_prec_block (pblk : prec_block) : string =
    let blk, p = pblk.pblk_block, pblk.pblk_pathcond in
    "Preceding BlockKey: { " ^ block_name blk ^ "; " ^ pr_predicate p ^ "}"
  ;;

  let pr_prec_blocks (pblks : prec_block list) : string =
    pr_items ~f:pr_prec_block pblks
  ;;

  let pr_succ_block (sblk : succ_block) : string =
    let blk, p = sblk.sblk_block, sblk.sblk_pathcond in
    "Succeeding BlockKey: { " ^ block_name blk ^ "; " ^ pr_predicate p ^ "}"
  ;;

  (*** Utility functions ***)

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
    Hashtbl.find_or_compute prog.prog_block_data.pbd_reachable_blocks
      ~f:compute ~key:blk
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
end

include Path

(*******************************************************************
 ** operations with functions and parameters
 *******************************************************************)

(** Module contains utility functions for processing functions *)
module Func = struct
  (*** Printing ***)

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
end

include Func

(*******************************************************************
 ** Utility functions for processing metadata
 *******************************************************************)

module Metadata = struct
  let extract_name_from_metadata (md : LL.llvalue) : string =
    let str = LL.string_of_llvalue md in
    let re = Str.regexp ".*name:[ ]*\"\\([a-zA-Z0-9_]+\\)\".*" in
    if Str.string_match re str 0 then Str.matched_group 1 str else ""
  ;;

  let get_original_name_of_llvalue (v : LL.llvalue) : string option =
    (* let _ = hprint "get_original_name_of_llvalue: " LI.pr_value v in *)
    match LL.classify_value v with
    | LV.Instruction _ ->
      let func = func_of_instr (mk_instr v) in
      let _ =
        iter_blocks
          ~f:(fun blk ->
            iter_instrs
              ~f:(fun instr ->
                if is_instr_call_invoke instr
                then (
                  let callee = callee_of_instr_func_call instr in
                  if is_func_llvm_debug_declare callee
                     || is_func_llvm_debug_value callee
                  then () (* hprint "Instr: " pr_instr instr *)
                  else ()))
              blk)
          func in
      None
    | _ -> None
  ;;

  let position_of_instr (instr : instr) : position option =
    let vinstr = llvalue_of_instr instr in
    let llctx = LL.global_context () in
    let instr_dbg = LL.metadata vinstr (LL.mdkind_id llctx "dbg") in
    match instr_dbg with
    | None -> None
    | Some instr_dbg_info ->
      let instr_md = LL.value_as_metadata instr_dbg_info in
      let line = LD.di_location_get_line ~location:instr_md in
      let column = LD.di_location_get_column ~location:instr_md in
      let scope_md = LD.di_location_get_scope ~location:instr_md in
      (* let _ = hprint "Line: " pr_int line in *)
      (* let _ = hprint "Column: " pr_int column in *)
      let filename =
        match LD.di_scope_get_file ~scope:scope_md with
        | None -> ""
        | Some file_md -> LD.di_file_get_filename ~file:file_md in
      (* let _ = hprint "Filename: " pr_id filename in *)
      Some (mk_position filename line line column column)
  ;;

  let pr_llvalue_name (v : LL.llvalue) : string =
    match get_original_name_of_llvalue v with
    | Some str -> str
    | None -> pr_value v
  ;;

  let pr_instr_location_and_code_excerpt instr =
    let code_excerpt =
      match position_of_instr instr with
      | None -> ""
      | Some p -> "  Location: " ^ pr_file_position_and_excerpt p ^ "\n" in
    if !location_source_code_only
    then code_excerpt
    else
      "  Instruction: " ^ pr_instr instr
      ^ String.prefix_if_not_empty ~prefix:"\n" code_excerpt
  ;;
end

include Metadata

(*******************************************************************
 ** Utility functions for processing modules
 *******************************************************************)

module Module = struct
  let pr_module (m : bitcode_module) : string = LL.string_of_llmodule m

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
    let process_global g =
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
    let process_param p =
      let vp = llvalue_of_param p in
      update_stats_of_llvalue vp in
    let process_instr i =
      let vi = llvalue_of_instr i in
      let _ = update_stats_of_llvalue vi in
      let _ = if is_instr_call_invoke i then incr num_func_calls in
      incr num_instrs in
    let process_block blk =
      let _ = incr num_blks in
      None in
    let process_func f =
      let _ = incr num_user_funcs in
      None in
    let _ =
      visit_module ~fglobal:(Some process_global) ~ffunc:(Some process_func)
        ~fparam:(Some process_param) ~fblock:(Some process_block)
        ~finstr:(Some process_instr) modul in
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
end

include Module

(*******************************************************************
 ** Utility functions for processing programs
 *******************************************************************)

module Program = struct
  (*** Printings ***)

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

  let construct_map_llvalue_to_source_name (prog : program) : unit =
    let _ = ddebug "Construct mapping llvalue to source name" in
    let process_instr instr =
      match instr_opcode instr with
      | LO.Call | LO.Invoke ->
        if is_func_llvm_debug (callee_of_instr_func_call instr)
        then (
          let _ = hprint "instr: " pr_instr instr in
          (* let v0, v1 = operand instr 0, operand instr 1 in *)
          let vname = pr_value (operand instr 0) in
          let sname = extract_name_from_metadata (operand instr 1) in
          Hashtbl.set prog.prog_meta_data.pmd_llvalue_original_name ~key:vname
            ~data:sname)
        else ()
      | _ -> () in
    visit_program ~finstr:(Some process_instr) prog
  ;;

  let compute_func_call_info (prog : program) : unit =
    let _ = ndebug "Compute function call information" in
    let equal = equal_func in
    let process_instr instr =
      match instr_opcode instr with
      | LO.Call | LO.Invoke ->
        let callee = callee_of_instr_func_call instr in
        let vcallee = llvalue_of_func callee in
        let caller = func_of_instr instr in
        let pfd = prog.prog_func_data in
        if is_llvalue_function vcallee
        then (
          let callers =
            List.insert_dedup (get_func_callers prog callee) caller ~equal
          in
          let _ = Hashtbl.set pfd.pfd_callers ~key:callee ~data:callers in
          let fcallees =
            List.insert_dedup (get_func_callees prog caller) callee ~equal
          in
          let callees = List.map ~f:mk_callable_func fcallees in
          Hashtbl.set pfd.pfd_callees ~key:caller ~data:callees)
        else (
          let fpcallees = get_func_ptr_callees prog caller in
          let fpcallees =
            List.insert_dedup fpcallees vcallee ~equal:equal_value in
          let callees = List.map ~f:mk_callable_func_pointer fpcallees in
          Hashtbl.set pfd.pfd_callees ~key:caller ~data:callees)
      | _ -> () in
    visit_program ~finstr:(Some process_instr) prog
  ;;

  let construct_func_call_graph (prog : program) : unit =
    let pfd = prog.prog_func_data in
    Hashtbl.iteri
      ~f:(fun ~key:func ~data:callees ->
        List.iter
          ~f:(fun callee ->
            match callee with
            | ClFunc fcallee ->
              CG.add_edge pfd.pfd_func_call_graph func fcallee
            | _ -> ())
          callees)
      pfd.pfd_callees
  ;;

  let compute_funcs_in_pointers (prog : program) : unit =
    let _ = ndebug "Compute functions in pointers" in
    let process_instr instr =
      let func =
        match instr_opcode instr with
        | LO.BitCast ->
          let v = src_of_instr_bitcast instr in
          if is_llvalue_function v then Some (mk_func v) else None
        | LO.Store ->
          let v = src_of_instr_store instr in
          if is_llvalue_function v then Some (mk_func v) else None
        | _ -> None in
      match func with
      | None -> ()
      | Some f ->
        let pfd = prog.prog_func_data in
        let ftyp = func_type f in
        let curr_funcs =
          match Hashtbl.find pfd.pfd_funcs_of_type ftyp with
          | None -> []
          | Some fs -> fs in
        let nfuncs = List.insert_dedup curr_funcs f ~equal:equal_func in
        Hashtbl.set pfd.pfd_funcs_of_type ~key:ftyp ~data:nfuncs in
    visit_program ~finstr:(Some process_instr) prog
  ;;

  let compute_func_used_globals (prog : program) : unit =
    let _ = print "Compute used globals in each functions" in
    let pfd = prog.prog_func_data in
    let tbl_used_globals = pfd.pfd_used_globals in
    let init_globals_of_all_funcs () =
      let _ =
        List.iter
          ~f:(fun func ->
            let gs = List.sorti prog.prog_globals ~compare:Poly.compare in
            Hashtbl.set tbl_used_globals ~key:func ~data:gs)
          prog.prog_init_funcs in
      List.iter
        ~f:(fun func ->
          let gs = ref [] in
          let process_instr instr =
            for i = 0 to num_operands instr - 1 do
              let opr = operand instr i in
              match LL.classify_value opr with
              | LV.GlobalVariable ->
                let gopr = mk_global opr in
                gs := List.insert_sorti_dedup !gs gopr ~compare:Poly.compare
              | _ -> ()
            done in
          let _ = visit_func ~finstr:(Some process_instr) func in
          Hashtbl.set tbl_used_globals ~key:func ~data:!gs)
        prog.prog_user_funcs in
    let update_globals_of_all_funcs () =
      let funcs = prog.prog_init_funcs @ prog.prog_user_funcs in
      let rec update_globals fs acc =
        let compare = Poly.compare in
        match fs with
        | [] -> acc
        | f :: nfs ->
          (* let _ = hprint "Update globals of func: " func_name f in *)
          let gs = Hashtbl.find_default tbl_used_globals f ~default:[] in
          let ngs = ref gs in
          let callees =
            let callees = get_func_callees prog f in
            if not !dfa_used_globals_in_func_ptrs
            then callees
            else (
              let ptr_callees = get_func_ptr_callees prog f in
              if List.is_empty ptr_callees
              then callees
              else
                List.fold_left
                  ~f:(fun acc pc ->
                    let pfd = prog.prog_func_data in
                    let ftyp = LL.type_of pc in
                    match Hashtbl.find pfd.pfd_funcs_of_type ftyp with
                    | None -> acc
                    | Some fs -> List.concat_dedup acc fs ~equal:equal_func)
                  ptr_callees ~init:callees) in
          let cgs =
            List.fold_left
              ~f:(fun acc f1 ->
                let gs1 =
                  Hashtbl.find_default tbl_used_globals f1 ~default:[] in
                List.concat_sorti_dedup acc gs1 ~compare)
              ~init:[] callees in
          let _ = ngs := List.concat_sorti_dedup !ngs cgs ~compare in
          (* let _ = print "  done" in *)
          if List.length !ngs > List.length gs
          then (
            let _ = Hashtbl.set tbl_used_globals ~key:f ~data:!ngs in
            update_globals nfs true)
          else update_globals nfs acc in
      let updated = ref true in
      while !updated do
        updated := update_globals funcs false
      done in
    let _ = init_globals_of_all_funcs () in
    update_globals_of_all_funcs ()
  ;;

  let update_program_info (prog : program) : program =
    let modul = prog.prog_bitcode_module in
    let prog =
      { prog with
        prog_all_funcs = get_all_funcs modul;
        prog_discover_funcs = get_discover_funcs modul;
        prog_lib_no_source_funcs = get_lib_no_source_funcs modul;
        prog_lib_has_source_funcs = get_lib_has_source_funcs modul;
        prog_user_funcs = get_user_funcs modul;
        prog_init_funcs = get_init_funcs modul;
        prog_entry_funcs = []
      } in
    let _ = print "Updating program information..." in
    let _ = compute_funcs_in_pointers prog in
    let _ = compute_func_call_info prog in
    let _ = compute_func_used_globals prog in
    let _ = construct_func_call_graph prog in
    prog
  ;;
end

include Program

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
