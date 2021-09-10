(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

exception Invalid_Z3_val

(*******************************************************************
 ** types
 *******************************************************************)

type z3_val =
  | Int of int
  | Frac of (float * float)

type z3_res =
  | Unsat
  | Sat of (string * z3_val) list
  | Error of string
  | Unk

(*******************************************************************
 ** printing
 *******************************************************************)

let pr_z3_val = function
  | Int i -> string_of_int i
  | Frac (f1, f2) -> (string_of_float f1) ^ "/" ^ (string_of_float f2)

let pr_z3_res = function
  | Unsat -> "unsat"
  | Sat m -> "sat: " ^ (pr_list (pr_pair (fun s -> s) pr_z3_val) m)
  | Unk   -> "unknown"
  | Error s -> "error: " ^ s

(*******************************************************************
 ** utilities
 *******************************************************************)

let z3_val_mult v1 v2 =
  match v1, v2 with
  | Int i1, Int i2 -> Int (i1 * i2)
  | Int i1, Frac (n2, d2) -> Frac ((float_of_int i1) *. n2, d2)
  | Frac (n1, d1), Int i2 -> Frac (n1 *. (float_of_int i2), d1)
  | Frac (n1, d1), Frac (n2, d2) -> Frac (n1 *. n2, d1 *. d2)

let z3_val_neg v =
  match v with
  | Int i -> Int (-i)
  | Frac (n, d) -> Frac (-.n, d)

let z3_val_to_int (vl: z3_val list): int list =
  match vl with
  | [] -> []
  | _ ->
    let int_of_float (f: float) =
      let i = truncate f in
      if Float.(=) (float_of_int i) f then i
      else raise Invalid_Z3_val in
    let den_l = List.fold_left ~f:(fun a v -> match v with
      | Int _ -> a | Frac (_, d) -> a @ [int_of_float d]) ~init:[] vl in
    let den_lcm = lcm_ints den_l in
    List.map ~f:(fun v -> match v with
      | Int i -> i * den_lcm
      | Frac (n, d) -> (int_of_float n) * den_lcm / (int_of_float d)) vl

let norm_model (model: (string * z3_val) list) : (string * int) list =
  let ss, vs = List.unzip model in
  let vs = z3_val_to_int vs in
  List.zip_exn ss vs
