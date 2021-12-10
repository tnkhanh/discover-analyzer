(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llprogram

module LL = Llvm

(*******************************************************************
 * Iteration, Mapping, Fold linearly over LLVM data structures
 *******************************************************************)

(** Iterate linearly over LLVM data structures *)

module IterLinear = struct
  (* let iter_uses *)

  let iter_instrs ~(f : instr -> unit) (blk : block) : unit =
    let ff v = f (mk_instr v) in
    LL.iter_instrs ff blk
  ;;

  let iter_blocks ~(f : block -> unit) (func : func) : unit =
    LL.iter_blocks f (llvalue_of_func func)
  ;;

  let iter_params ~(f : param -> unit) (func : func) : unit =
    let ff v = f (mk_param v) in
    LL.iter_params ff (llvalue_of_func func)
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

(** Map linearly over LLVM data structures *)

module MapLinear = struct
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

(** Fold linearly over LLVM data structures *)

module FoldLinear = struct
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

(** Check existence linearly over LLVM data structures *)

module ExistLinear = struct
  let iter_instrs ~(f : instr -> unit) (blk : block) : unit =
    let ff v = f (mk_instr v) in
    LL.iter_instrs ff blk
  ;;

  let iter_blocks ~(f : block -> unit) (func : func) : unit =
    LL.iter_blocks f (llvalue_of_func func)
  ;;

  let iter_params ~(f : param -> unit) (func : func) : unit =
    let ff v = f (mk_param v) in
    LL.iter_params ff (llvalue_of_func func)
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

include IterLinear
include MapLinear
include FoldLinear

(*******************************************************************
 * Iteration, Mapping, Fold structurally over LLVM data structures
 *******************************************************************)

(** Iteration LLVM data structures by following AST structure *)

module IterStructure = struct
  let iter_struct_instr
      ?(finstr : (instr -> unit) option = None)
      (instr : instr)
      : unit
    =
    match finstr with
    | None -> ()
    | Some f -> f instr
  ;;

  let iter_struct_param
      ?(fparam : (param -> unit) option = None)
      (param : param)
      : unit
    =
    match fparam with
    | None -> ()
    | Some f -> f param
  ;;

  let iter_struct_global
      ?(fglobal : (global -> unit) option = None)
      (glob : global)
      : unit
    =
    match fglobal with
    | None -> ()
    | Some f -> f glob
  ;;

  let iter_struct_block
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (blk : block)
      : unit
    =
    let iter () =
      LL.iter_instrs (fun i -> iter_struct_instr ~finstr (mk_instr i)) blk
    in
    let open Option.Let_syntax in
    let res =
      let%bind f = fblock in
      f blk in
    Option.value res ~default:(iter ())
  ;;

  let iter_struct_func
      ?(ffunc : (func -> unit option) option = None)
      ?(fparam : (param -> unit) option = None)
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (func : func)
      : unit
    =
    let iter () =
      let vfunc = llvalue_of_func func in
      let _ =
        LL.iter_params (fun p -> iter_struct_param ~fparam (mk_param p)) vfunc
      in
      LL.iter_blocks (iter_struct_block ~fblock ~finstr) vfunc in
    let open Option.Let_syntax in
    let res =
      let%bind f = ffunc in
      f func in
    Option.value res ~default:(iter ())
  ;;

  let iter_struct_module
      ?(fglobal : (global -> unit) option = None)
      ?(ffunc : (func -> unit option) option = None)
      ?(fparam : (param -> unit) option = None)
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (m : bitcode_module)
      : unit
    =
    LL.iter_globals (fun g -> iter_struct_global ~fglobal (mk_global g)) m;
    LL.iter_functions
      (fun f -> iter_struct_func ~ffunc ~fparam ~fblock ~finstr (mk_func f))
      m
  ;;

  let iter_struct_program
      ?(fglobal : (global -> unit) option = None)
      ?(ffunc : (func -> unit option) option = None)
      ?(fparam : (param -> unit) option = None)
      ?(fblock : (block -> unit option) option = None)
      ?(finstr : (instr -> unit) option = None)
      (prog : program)
      : unit
    =
    List.iter ~f:(iter_struct_global ~fglobal) prog.prog_globals;
    List.iter
      ~f:(iter_struct_func ~ffunc ~fparam ~fblock ~finstr)
      (prog.prog_init_funcs @ prog.prog_user_funcs)
  ;;
end

(** Fold LLVM data structures by following AST structure *)

module FoldStructure = struct
  let fold_struct_instr
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (instr : instr)
      : 'a
    =
    match finstr with
    | None -> acc
    | Some f -> f acc instr
  ;;

  let fold_struct_param
      ?(fparam : ('a -> param -> 'a) option = None)
      (acc : 'a)
      (param : param)
      : 'a
    =
    match fparam with
    | None -> acc
    | Some f -> f acc param
  ;;

  let fold_struct_global
      ?(fglobal : ('a -> global -> 'a) option = None)
      (acc : 'a)
      (glob : global)
      : 'a
    =
    match fglobal with
    | None -> acc
    | Some f -> f acc glob
  ;;

  let fold_struct_block
      ?(fblock : ('a -> block -> 'a option) option = None)
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (blk : block)
      : 'a
    =
    let fold () =
      LL.fold_left_instrs
        (fun acc' instr -> fold_struct_instr ~finstr acc' (mk_instr instr))
        acc blk in
    let open Option.Let_syntax in
    let res =
      let%bind f = fblock in
      f acc blk in
    Option.value res ~default:(fold ())
  ;;

  let fold_struct_func
      ?(ffunc : ('a -> func -> 'a option) option = None)
      ?(fparam : ('a -> param -> 'a) option = None)
      ?(fblock : ('a -> block -> 'a option) option = None)
      ?(finstr : ('a -> instr -> 'a) option = None)
      (acc : 'a)
      (func : func)
      : 'a
    =
    let fold () =
      let vfunc = llvalue_of_func func in
      let res =
        LL.fold_left_params
          (fun acc' param -> fold_struct_param ~fparam acc' (mk_param param))
          acc vfunc in
      LL.fold_left_blocks
        (fun acc' blk -> fold_struct_block ~fblock ~finstr acc' blk)
        res vfunc in
    let open Option.Let_syntax in
    let res =
      let%bind f = ffunc in
      f acc func in
    Option.value res ~default:(fold ())
  ;;

  let fold_struct_module
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
      LL.fold_left_globals
        (fun acc' glob -> fold_struct_global ~fglobal acc' (mk_global glob))
        acc m in
    let res =
      fold_left_functions
        ~f:(fun acc' fn ->
          fold_struct_func ~ffunc ~fparam ~fblock ~finstr acc' fn)
        ~init:res m in
    res
  ;;

  let fold_struct_program
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
      List.fold_left
        ~f:(fold_struct_global ~fglobal)
        ~init:acc prog.prog_globals in
    let res =
      List.fold_left
        ~f:(fold_struct_func ~ffunc ~fparam ~fblock ~finstr)
        ~init:res
        (prog.prog_init_funcs @ prog.prog_user_funcs) in
    res
  ;;
end

include IterStructure
include FoldStructure
