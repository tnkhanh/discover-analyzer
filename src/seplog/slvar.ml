(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover

type typ =
  | TVoid
  | TBool
  | TInt of int (* bitwidth *)
  | TFloat
  | TString
  | TUnk
  | TVar of int
  | TStruct of string
  | TPointer of typ
  | TArray
  | TFunc of (typ list * typ)
[@@deriving equal]

type identifier = string * int option
type var = identifier * typ
type vars = var list

(*******************************************************************
 ** Global variables
 *******************************************************************)

let index_var = ref 0

(*******************************************************************
 ** Printing
 *******************************************************************)

let pr_id (vn : identifier) : string =
  let base, index = vn in
  match index with
  | None -> base
  | Some i -> base ^ pr_int i
;;

let rec pr_type (t : typ) : string =
  match t with
  | TVoid -> "void"
  | TBool -> "bool"
  | TInt i -> "i" ^ pr_int i
  | TFloat -> "float"
  | TString -> "string"
  | TStruct n -> n
  | TPointer t -> pr_type t ^ "*"
  | TArray -> "array"
  | TFunc (ts, t) -> pr_types ts ^ " -> " ^ pr_type t
  | TVar i -> "tvar" ^ pr_int i
  | TUnk -> "unk"

and pr_types (ts : typ list) : string = pr_list ~sep:" * " ~f:pr_type ts

let pr_var (v : var) : string =
  let id, typ = v in
  if not !print_type
  then pr_id id
  else "(" ^ pr_id id ^ ":" ^ pr_type typ ^ ")"
;;

let pr_vars (vs : var list) : string = pr_list ~sep:"," ~f:pr_var vs

(*******************************************************************
 ** Constructors
 *******************************************************************)

let mk_var ?(index = None) (name : string) (typ : typ) : var =
  (name, index), typ
;;

let fresh_var_index () = index_var := !index_var + 1

let fresh_new_var ?(name = "t") (typ : typ) : var =
  let _ = fresh_var_index () in
  (name, Some !index_var), typ
;;

let fresh_old_var (v : var) : var =
  let (base, _), typ = v in
  let _ = fresh_var_index () in
  (base, Some !index_var), typ
;;

let typ_of_var (v : var) = snd v

let compare_var (v1 : var) (v2 : var) =
  let (base1, index1), (base2, index2) = fst v1, fst v2 in
  let cmp = String.compare base1 base2 in
  if cmp <> 0
  then cmp
  else (
    match index1, index2 with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some i, Some j -> if i < j then -1 else if i > j then 1 else 0)
;;

let equal_var v1 v2 = compare_var v1 v2 = 0
let member_vs v vs = List.exists ~f:(fun x -> equal_var v x) vs
let insert_vs v vs = if member_vs v vs then vs else v :: vs
let diff_vs vs1 vs2 = List.exclude ~f:(fun v -> member_vs v vs2) vs1
let intersect_vs vs1 vs2 = List.filter ~f:(fun v -> member_vs v vs2) vs1
let intersected_vs vs1 vs2 = not (List.is_empty (intersect_vs vs1 vs2))
let subset_vs vs1 vs2 = List.for_all ~f:(fun v -> member_vs v vs2) vs1

let merge_vs vss =
  List.fold_left ~f:(fun acc vs -> acc @ diff_vs vs acc) ~init:[] vss
;;

let dedup_vs vs =
  let rec dedup xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> if member_vs x acc then dedup xs acc else dedup xs (x :: acc)
  in
  dedup vs []
;;

(*******************************************************************
 ** functions with type
 *******************************************************************)

let is_typ_var t =
  match t with
  | TVar _ -> true
  | _ -> false
;;

let is_typ_known t =
  match t with
  | TVar _ -> false
  | _ -> true
;;
