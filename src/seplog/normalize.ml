(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Libdiscover
open Slir
module LA = Linarith
module SMT = Smt.SmtSL

(*******************************************************************
 ** normalize tautology and contradiction
 *******************************************************************)

let simplify_tauto_contra_p (f : pure_form) : pure_form * bool =
  let changed = ref false in
  let return_changed res =
    let _ = changed := true in
    res in
  let rec simplify p =
    match p with
    | Bool _ | BExp _ -> p
    | BEq (e, p) -> BEq (e, simplify p)
    | Reln (Eq, [ a; b ]) when equal_exp a b -> return_changed (Bool true)
    | Reln (Eq, [ Func (Sub, [ a; b ], _); Int 0 ]) when equal_exp a b ->
      return_changed (Bool true)
    | Reln (Ne, [ a; b ]) when equal_exp a b -> return_changed (Bool false)
    | Reln _ -> p
    | PNeg g ->
      (match simplify g with
      | Bool true -> return_changed (Bool false)
      | Bool false -> return_changed (Bool true)
      | ng -> mk_pneg ng)
    | PConj gs ->
      let ngs = gs |> List.map ~f:simplify |> List.exclude ~f:is_pf_true in
      if List.length ngs > 1 && List.exists ~f:is_pf_false ngs
      then return_changed (mk_pf_false ())
      else if List.length ngs = List.length gs
      then mk_pconj ngs
      else return_changed (mk_pconj ngs)
    | PDisj gs ->
      let ngs = gs |> List.map ~f:simplify |> List.exclude ~f:is_pf_false in
      if List.length ngs > 1 && List.exists ~f:is_pf_true ngs
      then return_changed (mk_pf_true ())
      else if List.length ngs = List.length gs
      then mk_pdisj ngs
      else mk_pdisj ngs
    | PExists (vs, g) -> mk_pexists vs (simplify g)
    | PForall (vs, g) -> mk_pforall vs (simplify g) in
  simplify f, !changed
;;

let simplify_tauto_contra_f (f : formula) : formula * bool =
  let changed = ref false in
  let return_changed res =
    let _ = changed := true in
    res in
  let rec simplify f =
    match f with
    | Pure p ->
      let p, is_changed = simplify_tauto_contra_p p in
      let _ = changed := is_changed in
      if is_pf_false p
      then mk_f_pure p
      else if SMT.is_unsat [ p ]
      then return_changed (mk_f_false ())
      else mk_f_pure p
    | Emp | Data _ | View _ | Iter _ | Array _ -> f
    | Star fs ->
      let nfs =
        fs
        |> List.map ~f:simplify
        |> List.exclude ~f:(fun f -> is_f_true f || is_f_emp f) in
      let _ = if List.length nfs < List.length fs then changed := true in
      mk_f_star nfs
    | Wand (f1, f2) ->
      let f1, f2 = simplify f1, simplify f2 in
      if is_f_emp f1 || is_f_true f1 then return_changed f2 else mk_f_wand f1 f2
    | Septract (f1, f2) -> mk_f_septract (simplify f1) (simplify f2)
    | Exists (vs, g) -> mk_f_exists vs (simplify g) in
  simplify f, !changed
;;

(*******************************************************************
 ** normalize universal and existential variables
 *******************************************************************)

let eliminate_exists_var_by_equality_p ?(keep_vars = []) f : pure_form =
  let rec simplify p =
    match p with
    | Bool _ | BExp _ | BEq _ | Reln _ -> p
    | PNeg g -> g |> simplify |> mk_pneg
    | PConj gs -> gs |> List.map ~f:simplify |> mk_pconj
    | PDisj gs -> gs |> List.map ~f:simplify |> mk_pdisj
    | PForall (vs, g) -> mk_pforall vs (simplify g)
    | PExists (vs, g) ->
      let g = simplify g in
      let eqs = collect_eq_exp_p g in
      let sst = get_subst_of_vars vs eqs in
      let g, _ = g |> substitute_pure_form sst |> simplify_tauto_contra_p in
      let nvs = intersect_vs vs (fv_pf g) in
      mk_pexists nvs g in
  simplify f
;;

let eliminate_exists_var_by_equality_f ?(keep_vars = []) f : formula =
  let rec simplify f =
    match f with
    | Pure p -> Pure (eliminate_exists_var_by_equality_p ~keep_vars p)
    | Emp | Data _ | View _ | Iter _ | Array _ -> f
    | Star fs -> mk_f_star (List.map ~f:simplify fs)
    | Wand (f1, f2) -> mk_f_wand (simplify f1) (simplify f2)
    | Septract (f1, f2) -> mk_f_septract (simplify f1) (simplify f2)
    | Exists (vs, g) ->
      let g = simplify g in
      let eqs = collect_eq_exp_f g in
      let sst = get_subst_of_vars vs eqs in
      let g, _ = g |> substitute_formula sst |> simplify_tauto_contra_f in
      mk_f_exists vs g in
  simplify f
;;

(*******************************************************************
 ** normalize equality
 *******************************************************************)

let substitute_var_by_equality_p keep_vars f : pure_form =
  let rec simplify kvs p =
    match p with
    | Bool _ | BExp _ | BEq _ | Reln _ -> p
    | PNeg g -> g |> simplify kvs |> mk_pneg
    | PConj gs ->
      let g = gs |> List.map ~f:(simplify kvs) |> mk_pconj in
      let eqs =
        let eqs = collect_eq_exp_p g in
        List.filter
          ~f:(function
            | EqExp (_, e) -> subset_vs (fv_e e) kvs
            | EqPure (_, p) -> subset_vs (fv_pf p) kvs)
          eqs in
      let sst = get_substitute_formula_eqs eqs in
      g |> substitute_pure_form sst |> simplify_tauto_contra_p |> fst
    | PDisj gs -> gs |> List.map ~f:(simplify kvs) |> mk_pdisj
    | PForall (vs, g) ->
      let g = simplify (diff_vs kvs vs) g in
      mk_pforall vs g
    | PExists (vs, g) ->
      let g = simplify (diff_vs kvs vs) g in
      mk_pexists vs g in
  simplify keep_vars f
;;

let substitute_var_by_equality_f keep_vars f : formula =
  let rec simplify kvs f =
    match f with
    | Pure p -> Pure (substitute_var_by_equality_p kvs p)
    | Emp | Data _ | View _ | Iter _ | Array _ -> f
    | Star fs ->
      let g = mk_f_star (List.map ~f:(simplify kvs) fs) in
      let eqs =
        let eqs = collect_eq_exp_f g in
        List.filter
          ~f:(function
            | EqExp (_, e) -> subset_vs (fv_e e) kvs
            | EqPure (_, p) -> subset_vs (fv_pf p) kvs)
          eqs in
      let sst = get_substitute_formula_eqs eqs in
      g |> substitute_formula sst |> simplify_tauto_contra_f |> fst
    | Wand (f1, f2) -> mk_f_wand (simplify kvs f1) (simplify kvs f2)
    | Septract (f1, f2) -> mk_f_septract (simplify kvs f1) (simplify kvs f2)
    | Exists (vs, g) ->
      let g = simplify (diff_vs kvs vs) g in
      mk_f_exists vs g in
  simplify keep_vars f
;;

(*******************************************************************
 ** simplify arithmetic
 *******************************************************************)

let simplify_arith_e e =
  let rec simplify e =
    match e with
    | Func (func, [ e1; e2 ], t) ->
      if equal_func_symbol func Add
         || equal_func_symbol func Sub
         || equal_func_symbol func Mul
      then (
        match LA.term_of_exp e with
        | t, None -> LA.exp_of_term t
        | _ -> e)
      else (
        (* op = Div *)
        let e2 = simplify e2 in
        let res =
          match e2 with
          | Int 1 -> e1
          | _ -> Func (func, [ e1; e2 ], t) in
        res)
    | _ -> e in
  simplify e
;;

let simplify_arith_es es = List.map ~f:simplify_arith_e es

let simplify_arith_p (f : pure_form) =
  let mk_reln_lterm_int reln lt i =
    let cvs, k = lt in
    let cvs1, cvs2 = List.partition_tf ~f:(fun (c, v) -> c = 1) cvs in
    match cvs1 with
    | (c, v) :: ncvs1 ->
      let e1 = mk_exp_var v in
      let ncvs = List.map ~f:(fun (c, v) -> -c, v) (ncvs1 @ cvs2) in
      let e2 = LA.exp_of_term (ncvs, i - k) in
      mk_preln reln [ e1; e2 ]
    | _ ->
      let e1 = LA.exp_of_term lt in
      let e2 = mk_exp_int i in
      mk_preln reln [ e1; e2 ] in
  let simplify_reln reln e1 e2 =
    let e1, e2 = simplify_arith_e e1, simplify_arith_e e2 in
    match LA.term_of_exp (mk_sub e1 e2) with
    | (cvs, i), None ->
      let cs, vs = cvs |> List.unzip in
      let k = i :: cs |> Math.gcd_ints |> abs in
      if k > 1
      then (
        let ncs = cs |> List.map ~f:(fun x -> x / k) in
        let ncvs = List.zip_exn ncs vs in
        let nt = ncvs, i / k in
        mk_reln_lterm_int reln nt 0)
      else mk_reln_lterm_int reln (cvs, i) 0
    | _ -> Reln (reln, [ e1; e2 ]) in
  let rec simplify g =
    match g with
    | Bool _ | BExp _ -> g
    | Reln (reln, [ e1; e2 ]) when is_int_exp e1 -> simplify_reln reln e1 e2
    | Reln _ -> g
    | PNeg g -> PNeg (simplify g)
    | BEq (e, g) -> BEq (e, simplify g)
    | PConj gs -> PConj (List.map ~f:simplify gs)
    | PDisj gs -> PDisj (List.map ~f:simplify gs)
    | PForall (vs, g) -> PForall (vs, simplify g)
    | PExists (vs, g) -> PExists (vs, simplify g) in
  simplify f
;;

let simplify_arith_ad a =
  { addr_base = simplify_arith_e a.addr_base
  ; addr_elem = simplify_arith_e a.addr_elem
  ; addr_field = simplify_arith_e a.addr_field
  }
;;

let simplify_arith_sf d =
  { d with
    data_root = simplify_arith_e d.data_root
  ; data_args = simplify_arith_es d.data_args
  ; data_addr = Option.map ~f:simplify_arith_ad d.data_addr
  }
;;

let simplify_arith_vf v = { v with view_args = simplify_arith_es v.view_args }

let simplify_arith_if i =
  { i with
    iter_base = simplify_arith_e i.iter_base
  ; iter_element_index = simplify_arith_e i.iter_element_index
  ; iter_begin = simplify_arith_e i.iter_begin
  ; iter_end = simplify_arith_e i.iter_end
  }
;;

let simplify_arith_af a =
  { a with
    array_root = simplify_arith_e a.array_root
  ; array_size = simplify_arith_e a.array_size
  }
;;

let simplify_arith_f (f : formula) : formula =
  let rec simplify f =
    match f with
    | Emp -> f
    | Data d -> Data (simplify_arith_sf d)
    | View v -> View (simplify_arith_vf v)
    | Iter a -> Iter (simplify_arith_if a)
    | Array a -> Array (simplify_arith_af a)
    | Pure p -> Pure (simplify_arith_p p)
    | Star fs -> Star (List.map ~f:simplify fs)
    | Wand (f1, f2) -> mk_f_wand (simplify f1) (simplify f2)
    | Septract (f1, f2) -> mk_f_septract (simplify f1) (simplify f2)
    | Exists (vs, f) -> Exists (vs, simplify f) in
  simplify f
;;

(*******************************************************************
 ** normalize wand
 *******************************************************************)

(* x->y * (exists z. (x->z --* (x->_ * F)))  <=>  x->y * F  *)
(* x->y * (exists z. (x->z --* ((array(a,_) +* (x,a,_)->_) * F)))
   <=>  ((array(a,_) +* (x,a,_)->y) * F)  *)

(* let rec eliminate_wand_outer_data f =
 *   let rec eliminate f = match f with
 *     | Emp | Pure _ | Cell _ | Data _ | View _ | Iter _ | Array _ -> f
 *     | Wand (f1, f2) ->
 *       let f1, f2 = eliminate f1, eliminate f2 in
 *       (match f1 with
 *        | Data df as fd ->
 *          let fs = match f2 with | Star fs -> fs | _ -> [f2] in
 *          let fs1, fs2 = List.partition_tf ~f:(equal_df_form_name_root fd) fs in
 *          (match fs1 with
 *           | (Data df1)::nfs1 ->
 *             let eq_args = mk_eq_exps_pair df.data_args df1.data_args in
 *             mk_f_star (nfs1 @ fs2 @ [mk_f_pure eq_args])
 *           | _ -> mk_f_wand f1 f2)
 *        | _ -> mk_f_wand f1 f2)
 *     | Septract (f1, f2) -> Septract (eliminate f1, eliminate f2)
 *     | Star fs -> fs |> List.map ~f:eliminate |> mk_f_star
 *     | Exists (vs, f) -> f |> eliminate |> mk_f_exists vs in
 *   let nf = eliminate f in
 *   debug_n ("eliminate_wand_outer_data:\n" ^
 *              " before: " ^ (sprint_f f) ^ "\n" ^
 *              " after : " ^ (sprint_f nf));
 *   nf *)

(*******************************************************************
 ** normalize array
 *******************************************************************)

(* array(x,y,t) --* (array(x,z,t) * F)  <==> F & y=z *)

(* let rec eliminate_wand_array f =
 *   let rec eliminate f = match f with
 *     | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> f
 *     | Wand (f1, f2) ->
 *       let f1, f2 = eliminate f1, eliminate f2 in
 *       (match f1 with
 *        | Array af as fa ->
 *          let fs = match f2 with | Star fs -> fs | _ -> [f2] in
 *          let fs1, fs2 = List.partition_tf ~f:(eq_af_form_root_typ fa) fs in
 *          (match fs1 with
 *           | (Array af1)::nfs1 ->
 *             let equal_size = mk_eq af.array_size af1.array_size in
 *             mk_f_star (nfs1 @ fs2 @ [mk_f_pure equal_size])
 *           | _ -> mk_f_wand f1 f2)
 *        | _ ->
 *          mk_f_wand f1 f2)
 *     | Septract (f1, f2) -> Septract (eliminate f1, eliminate f2)
 *     | Star fs -> fs |> List.map ~f:eliminate |> mk_f_star
 *     | Exists (vs, f) -> f |> eliminate |> mk_f_exists vs in
 *   let nf = eliminate f in
 *   debug_n ("eliminate_wand_array:\n" ^
 *              " before: " ^ (sprint_f f) ^ "\n" ^
 *              " after : " ^ (sprint_f nf));
 *   nf *)

(* (array(x,y,t) *- u->v) * u->t  <==> (array(x,z,t) *+ u->t) *)

(* let rec eliminate_septraction_array f =
 *   let rec extract_common_data dfs1 dfs2 adfs1 =
 *     match dfs1 with
 *     | [] -> None
 *     | df1::ndfs1 ->
 *       let dfs2_extracted, dfs2_others = List.partition_tf ~f:(fun df2 ->
 *         equal_exp df1.data_root df2.data_root) dfs2 in
 *       match dfs2_extracted with
 *       | [] -> extract_common_data ndfs1 dfs2 (adfs1 @ [df1])
 *       | df2::ndfs2 ->
 *         let dfs1_others = adfs1 @ ndfs1 in
 *         let dfs2_others = ndfs2 @ dfs2_others in
 *         Some (df1, df2, dfs1_others, dfs2_others) in
 *   let rec merge_data_array dfs afs aafs =
 *     match afs with
 *     | [] -> mk_f_star_with (mk_f_arrays aafs) ~dfs
 *     | af::nafs ->
 *       match extract_common_data dfs af.array_subtract [] with
 *       | None -> merge_data_array dfs nafs (aafs @ [af])
 *       | Some (df, df_subtract, other_dfs, other_dfs_subtract) ->
 *         let df_update = {df_subtract with data_args = df.data_args} in
 *         let naf = {af with array_update = af.array_update @ [df_update];
 *                            array_subtract = other_dfs_subtract} in
 *         merge_data_array other_dfs (naf::nafs) aafs in
 *   let rec eliminate f = match f with
 *     | Emp | Pure _ | Data _ | View _ | Iter _ | Array _
 *     | Wand _ | Septract _ -> f
 *     | Star fs ->
 *       let dfs, others = List.fold_left ~f:(fun (dfs, others) f ->
 *         match f with
 *         | Data d -> (dfs @ [d], others)
 *         | _ -> dfs, others @ [f]) ~init:([], []) fs in
 *       let afs, others = List.fold_left ~f:(fun (afs, others) f ->
 *         match f with
 *         | Array a -> (afs @ [a], others)
 *         | _ -> afs, others @ [f]) ~init:([], []) others in
 *       let res = merge_data_array dfs afs [] in
 *       let others = List.map ~f:eliminate others in
 *       mk_f_star_with (mk_f_star others) ~fs:[res]
 *     | Exists (vs, f) -> f |> eliminate |> mk_f_exists vs in
 *   let nf = eliminate f in
 *   debug_n ("eliminate_septraction_array:\n" ^
 *              " before: " ^ (sprint_f f) ^ "\n" ^
 *              " after : " ^ (sprint_f nf));
 *   nf *)

(* (\* array(x,1,t) * F  <==> F * x->t{_} *\)
 * let rec simplify_array_one_data f =
 *   let rec simplify f = match f with
 *     | Emp | Pure _ | Data _ | View _  | Iter _ -> f
 *     | Array af -> (match af.array_size with
 *       | Int (1, _) ->
 *         let arg = mk_exp_var (fresh_new_var ~name:"t" af.array_typ) in
 *         mk_f_data (mk_data af.array_typ af.array_root [arg])
 *       | _ -> Array af)
 *     | Wand (f1, f2) -> Wand (simplify f1, simplify f2)
 *     | Septract (f1, f2) -> Septract (simplify f1, simplify f2)
 *     | Star fs -> fs |> List.map ~f:simplify |> mk_f_star
 *     | Exists (vs, f) -> f |> simplify |> mk_f_exists vs in
 *   let nf = simplify f in
 *   debug_n ("simplify_array_one_data:\n" ^
 *              " before: " ^ (sprint_f f) ^ "\n" ^
 *              " after : " ^ (sprint_f nf));
 *   nf *)

(*******************************************************************
 ** pure encoding
 *******************************************************************)

let encode_formula_to_pure (f : formula) : pure_form =
  let rec collect_root f =
    match f with
    | Emp | View _ | Iter _ | Array _ | Pure _ | Wand _ | Septract _ -> []
    | Data d -> [ d.data_root ]
    | Star fs ->
      List.fold_left ~f:(fun acc f -> acc @ collect_root f) ~init:[] fs
    | Exists (_, f) -> collect_root f in
  let encode_disjointness f = f |> collect_root |> mk_ne_exps in
  let rec encode_sub_form f =
    match f with
    | Emp | View _ | Iter _ | Array _ -> mk_pf_true ()
    | Pure p -> p
    | Data d -> mk_ne d.data_root (mk_null ())
    | Star fs -> mk_pconj (List.map ~f:encode_sub_form fs)
    | Wand _ | Septract _ -> mk_pf_true ()
    | Exists (_, f) -> encode_sub_form f in
  let rec encode f =
    match f with
    | Exists (vs, f) -> mk_pexists vs (encode f)
    | _ -> mk_pconj [ encode_sub_form f; encode_disjointness f ] in
  encode f
;;

(*******************************************************************
 ** simplify all formulas
 *******************************************************************)

let simplify_p (f : pure_form) : pure_form = f

let simplify_f (f : formula) : formula =
  f
  |> (* eliminate_wand_array |> *)
     (* eliminate_septraction_array |> *)
     (* eliminate_wand_outer_data |> *)
  eliminate_exists_var_by_equality_f
  |> simplify_arith_f
  |> eliminate_exists_var_by_equality_f
  |> (* simplify_array_one_data |> *)
  simplify_tauto_contra_f
  |> fst
;;

let simplify_entail (e : entailment) : entailment =
  { e with ent_lhs = simplify_f e.ent_lhs }
;;

let simplify_entails (es : entailment list) : entailment list =
  List.map ~f:simplify_entail es
;;
