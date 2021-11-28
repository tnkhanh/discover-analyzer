(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Libdiscover
open Slir

(** each term is stored as a list of variables with their coefficients
    and the integer part *)
type term = (int * var) list * int

(** addition: t1 + t2
    prerequisite: terms' variables are ordered *)
let add_terms (t1 : term) (t2 : term) : term =
  let rec sum t1 t2 =
    match t1, t2 with
    | ([], i1), ([], i2) -> [], i1 + i2
    | (k, i1), ([], i2) -> k, i1 + i2
    | ([], i1), (k, i2) -> k, i1 + i2
    | ((c1, v1) :: k1, i1), ((c2, v2) :: k2, i2) ->
      if compare_var v1 v2 = 0
      then (
        let k, i = sum (k1, i1) (k2, i2) in
        if c1 + c2 = 0 then k, i else (c1 + c2, v1) :: k, i)
      else if compare_var v1 v2 < 0
      then (
        let k, i = sum (k1, i1) ((c2, v2) :: k2, i2) in
        (c1, v1) :: k, i)
      else (
        let k, i = sum ((c1, v1) :: k1, i1) (k2, i2) in
        (c2, v2) :: k, i) in
  sum t1 t2
;;

(** multiply term with coefficient *)
let mult_term_with_coefficient (t : term) (c : int) : term =
  let k, i = t in
  let nk = List.map ~f:(fun (t, v) -> t * c, v) k in
  nk, c * i
;;

(** subtraction: t1 - t2
    prerequisite: terms' variables are ordered *)
let subtract_term (t1 : term) (t2 : term) : term =
  let nt2 = mult_term_with_coefficient t2 (-1) in
  add_terms t1 nt2
;;

(** convert term to Cformula exp *)
let exp_of_term (t : term) : exp =
  let rec convert t =
    match t with
    | [], i -> if i = 0 then None else Some (mk_exp_int i)
    | (c, v) :: k, i ->
      let e1o = convert (k, i) in
      let sv = mk_exp_var v in
      (match e1o with
      | None ->
        if c = 0
        then None
        else if c = 1
        then Some sv
        else Some (mk_mul (mk_exp_int c) sv)
      | Some e1 ->
        if c = 0
        then Some e1
        else if c = 1
        then Some (mk_add sv e1)
        else if c = -1
        then Some (mk_sub e1 sv)
        else if c < 0
        then (
          let e2 = mk_mul (mk_exp_int (-c)) sv in
          Some (mk_sub e1 e2))
        else (
          let e2 = mk_mul (mk_exp_int c) sv in
          Some (mk_add e1 e2))) in
  let eopt = convert t in
  match eopt with
  | None -> mk_exp_int 0
  | Some e -> e
;;

(** convert Cformula exp to a sum of
    a linear arithmetic term and an inconvertible expression*)
let term_of_exp (e : exp) : term * exp option =
  let rec convert e =
    match e with
    | Int i -> ([], i), None
    | Var v -> ([ 1, v ], 0), None
    | Func (Add, [ e1; e2 ], _) ->
      let (k1, i1), e1o = convert e1 in
      let (k2, i2), e2o = convert e2 in
      let nt = add_terms (k1, i1) (k2, i2) in
      let neo =
        match e1o, e2o with
        | None, _ -> e2o
        | _, None -> e1o
        | Some ue1o, Some ue2o -> Some (mk_add ue1o ue2o) in
      nt, neo
    | Func (Sub, [ e1; e2 ], _) ->
      let (k1, i1), o1 = convert e1 in
      let (k2, i2), o2 = convert e2 in
      let nt = subtract_term (k1, i1) (k2, i2) in
      let neo =
        match o1, o2 with
        | None, None -> None
        | None, Some x2 -> Some (mk_sub (mk_exp_int 0) x2)
        | Some _, None -> o1
        | Some x1, Some x2 -> Some (mk_sub x1 x2) in
      nt, neo
    | Func (Mul, [ e1; e2 ], _) ->
      let (k1, i1), o1 = convert e1 in
      let (k2, i2), o2 = convert e2 in
      if List.is_empty k1 && Option.is_none o1
      then (
        let nt = mult_term_with_coefficient (k2, i2) i1 in
        let ne =
          match o2 with
          | None -> None
          | Some x2 ->
            if i1 = 0
            then None
            else if i1 = 1
            then o2
            else Some (mk_mul (mk_exp_int i1) x2) in
        nt, ne)
      else if List.is_empty k2 && Option.is_none o2
      then (
        let nt = mult_term_with_coefficient (k1, i1) i2 in
        let ne =
          match o1 with
          | None -> None
          | Some x1 ->
            if i2 = 0
            then None
            else if i2 = 1
            then o1
            else Some (mk_mul (mk_exp_int i2) x1) in
        nt, ne)
      else ([], 0), Some e
    | _ -> ([], 0), Some e in
  convert e
;;

let subtract_exp (e1 : exp) (e2 : exp) : term =
  let t1, o1 = term_of_exp e1 in
  let t2, o2 = term_of_exp e2 in
  match o1, o2 with
  | None, None -> subtract_term t1 t2
  | _, _ ->
    let msg = "subtract_exp: non-linear arguments are not allowed" in
    raise (Invalid_argument msg)
;;

let is_zero t =
  match fst t with
  | [] -> snd t = 0
  | _ -> false
;;

let is_non_zero t =
  match fst t with
  | [] -> snd t != 0
  | _ -> false
;;

let is_positive t =
  match fst t with
  | [] -> snd t > 0
  | _ -> false
;;

let is_non_negative t =
  match fst t with
  | [] -> snd t >= 0
  | _ -> false
;;

let is_negative t =
  match fst t with
  | [] -> snd t < 0
  | _ -> false
;;

let is_non_positive t =
  match fst t with
  | [] -> snd t <= 0
  | _ -> false
;;

(** simplify exp *)
let simplify_exp (e : exp) : exp =
  let t, o = term_of_exp e in
  let u = exp_of_term t in
  match u, o with
  | _, None -> u
  | Int i, Some x when i = 0 -> x
  | _, Some x -> mk_add u x
;;
