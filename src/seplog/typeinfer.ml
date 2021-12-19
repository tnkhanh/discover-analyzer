(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Slast

type environment = string -> typ
type substitution = typ -> typ

exception Typ_not_found

(* global vars *)

let tvar_index : int ref = ref 0

(* type utilities *)

let fresh_tvar () =
  let tvar = TVar !tvar_index in
  let _ = tvar_index := !tvar_index + 1 in
  tvar
;;

let reset_tvar_index () = tvar_index := 0
let new_typ_env () : environment = fun x -> TUnk

let extend_env (env : environment) (id : string) (t : typ) : environment =
 fun x -> if String.equal x id then t else env x
;;

let remove_all_vars_from_env (env : environment) : environment =
 fun n ->
  let t = env n in
  match t with
  | TFunc _ | TStruct _ -> t
  | _ -> TUnk
;;

let remove_vars_from_env (env : environment) (vs : var list) : environment =
  let ns = List.map ~f:pr_var vs in
  fun n -> if List.mem ~equal:String.equal ns n then TUnk else env n
;;

(* type substitutions *)

(* NOTE: maintain the environment function f which has the following property:
     for all x, ether f(x) = x
                or if f(x) = y with y!=x, then f(y) = y
   ==> Is it the IDEMPOTENT function? *)

let mk_subst_id () t = t

let update_substitution sst (old_typ : typ) (new_typ : typ) : substitution =
  let cur_typ = sst old_typ in
  let _ =
    if is_typ_known cur_typ && is_typ_known new_typ
       && not (equal_typ cur_typ new_typ)
    then
      error
        ("update_sst: unmatched current and new types: " ^ pr_type cur_typ
       ^ " ~~ " ^ pr_type new_typ) in
  fun t ->
    let substed_t = sst t in
    match cur_typ with
    | TVar _ ->
      if equal_typ t old_typ || equal_typ t cur_typ
         || equal_typ substed_t old_typ
         || equal_typ substed_t cur_typ
      then new_typ
      else substed_t
    | _ ->
      if equal_typ t old_typ || equal_typ t new_typ
         || equal_typ substed_t old_typ
         || equal_typ substed_t new_typ
      then cur_typ
      else substed_t
;;

let unify_typ sst current_typ expected_typ : substitution =
  if equal_typ current_typ expected_typ
  then sst
  else (
    match current_typ, expected_typ with
    | TUnk, _ -> error "unify_typ: current_typ: not expect TUnk"
    | _, TUnk -> error "unify_typ: expected_typ: not expect TUnk"
    | TVar _, TVar _ ->
      let t1, t2 = sst current_typ, sst expected_typ in
      if equal_typ t1 current_typ
      then update_substitution sst expected_typ t1
      else update_substitution sst current_typ t2
    | TVar _, _ -> update_substitution sst current_typ expected_typ
    | _, TVar _ -> update_substitution sst expected_typ current_typ
    | _, _ ->
      error
        ("unify_typ: found typ: " ^ pr_type current_typ ^ " but expect typ: "
       ^ pr_type expected_typ))
;;

(*******************************************************************
 ** annotate type formulas
 *******************************************************************)

let annotate_typ_var prog env (v : var) : var * environment =
  let vname, vtyp = v in
  let env_typ = env (pr_id vname) in
  match vtyp, env_typ with
  | TUnk, TUnk ->
    let ntyp = fresh_tvar () in
    let nv = vname, ntyp in
    let nenv = extend_env env (pr_id vname) ntyp in
    nv, nenv
  | TUnk, _ -> (vname, env_typ), env
  | _, TUnk -> v, extend_env env (pr_id vname) vtyp
  | _, _ ->
    if equal_typ env_typ vtyp
    then v, env
    else herror "annotate_typ_var: cannot annotate type of" pr_var v
;;

let annotate_typ_vars prog env vs : var list * environment =
  let vs, env =
    List.fold_left
      ~f:(fun (avs, env) v ->
        let v, env = annotate_typ_var prog env v in
        avs @ [ v ], env)
      ~init:([], env) vs in
  vs, env
;;

let rec annotate_typ_exp prog env e : exp * environment =
  match e with
  | Int _ | Float _ | String _ -> e, env
  | Var (v, l) ->
    let v, env = annotate_typ_var prog env v in
    Var (v, l), env
  | BinExp (op, e1, e2, l) ->
    let e1, env = annotate_typ_exp prog env e1 in
    let e2, env = annotate_typ_exp prog env e2 in
    BinExp (op, e1, e2, l), env
  | Func (fn, es, t, l) ->
    let es, env = annotate_typ_exps prog env es in
    let env =
      match env fn with
      | TUnk ->
        let param_typs = List.map ~f:typ_of_exp es in
        let ret_type = fresh_tvar () in
        let ftyp = TFunc (param_typs, ret_type) in
        extend_env env fn ftyp
      | _ -> env in
    Func (fn, es, t, l), env

and annotate_typ_exps prog env es : exp list * environment =
  let es, env =
    List.fold_left
      ~f:(fun (aes, env) e ->
        let e, env = annotate_typ_exp prog env e in
        aes @ [ e ], env)
      ~init:([], env) es in
  es, env
;;

let annotate_typ_addr_exp prog env a : addr_exp * environment =
  let base, env = annotate_typ_exp prog env a.addr_base in
  let elem, env = annotate_typ_exp prog env a.addr_elem in
  let field, env = annotate_typ_exp prog env a.addr_field in
  let addr = { addr_base = base; addr_elem = elem; addr_field = field } in
  addr, env
;;

let rec annotate_typ_formula prog env f : formula * environment =
  match f with
  | Bool _ -> f, env
  | Emp _ -> f, env
  | BVar (v, l) ->
    let v, env = annotate_typ_var prog env v in
    BVar (v, l), env
  | BinRel (rel, e1, e2, l) ->
    let e1, env = annotate_typ_exp prog env e1 in
    let e2, env = annotate_typ_exp prog env e2 in
    BinRel (rel, e1, e2, l), env
  | Data (e, t, es, addr, l) ->
    let e, env = annotate_typ_exp prog env e in
    let es, env = annotate_typ_exps prog env es in
    let addr, env =
      match addr with
      | None -> None, env
      | Some a ->
        let addr, env = annotate_typ_addr_exp prog env a in
        Some a, env in
    let dn = pr_type t in
    let env =
      match env dn with
      | TUnk ->
        let field_typs = List.map ~f:typ_of_exp es in
        let styp = TFunc (t :: field_typs, TBool) in
        extend_env env dn styp
      | _ -> env in
    Data (e, t, es, addr, l), env
  | Pred (pn, es, l) ->
    let es, env = annotate_typ_exps prog env es in
    let env =
      match env pn with
      | TUnk ->
        let param_typs = List.map ~f:typ_of_exp es in
        let ptyp = TFunc (param_typs, TBool) in
        extend_env env pn ptyp
      | _ -> env in
    Pred (pn, es, l), env
  | Array (root, size, etyp, l) ->
    let root, env = annotate_typ_exp prog env root in
    let size, env = annotate_typ_exp prog env size in
    Array (root, size, etyp, l), env
  | Neg g ->
    let g, env = annotate_typ_formula prog env g in
    Neg g, env
  | BEq (e, f) ->
    let e, env = annotate_typ_exp prog env e in
    let f, env = annotate_typ_formula prog env f in
    BEq (e, f), env
  | Conj (f1, f2) ->
    let f1, env = annotate_typ_formula prog env f1 in
    let f2, env = annotate_typ_formula prog env f2 in
    Conj (f1, f2), env
  | Disj (f1, f2) ->
    let f1, env = annotate_typ_formula prog env f1 in
    let f2, env = annotate_typ_formula prog env f2 in
    Disj (f1, f2), env
  | Star (f1, f2) ->
    let f1, env = annotate_typ_formula prog env f1 in
    let f2, env = annotate_typ_formula prog env f2 in
    Star (f1, f2), env
  | Wand (f1, f2) ->
    let f1, env = annotate_typ_formula prog env f1 in
    let f2, env = annotate_typ_formula prog env f2 in
    Wand (f1, f2), env
  | Septract (f1, f2) ->
    let f1, env = annotate_typ_formula prog env f1 in
    let f2, env = annotate_typ_formula prog env f2 in
    Septract (f1, f2), env
  | Update (f1, f2) ->
    let f1, env = annotate_typ_formula prog env f1 in
    let f2, env = annotate_typ_formula prog env f2 in
    Update (f1, f2), env
  | Forall (vs, f) ->
    let vs, env = annotate_typ_vars prog env vs in
    let f, env = annotate_typ_formula prog env f in
    let env = remove_vars_from_env env vs in
    Forall (vs, f), env
  | Exists (vs, f) ->
    let vs, env = annotate_typ_vars prog env vs in
    let f, env = annotate_typ_formula prog env f in
    let env = remove_vars_from_env env vs in
    Exists (vs, f), env

and annotate_typ_formulas prog env fs : formula list * environment =
  let fs, env =
    List.fold_left
      ~f:(fun (afs, env) f ->
        let f, env = annotate_typ_formula prog env f in
        afs @ [ f ], env)
      ~init:([], env) fs in
  fs, env
;;

let annotate_typ_entailment prog env ent : entailment * environment =
  let lhs, rhs = ent.ent_lhs, ent.ent_rhs in
  let lhs, env = annotate_typ_formula prog env lhs in
  let rhs, env = annotate_typ_formula prog env rhs in
  let env = remove_all_vars_from_env env in
  { ent with ent_lhs = lhs; ent_rhs = rhs }, env
;;

let annotate_typ_entailments prog env ents : entailment list * environment =
  let ents, env =
    List.fold_left
      ~f:(fun (aents, env) ent ->
        let ent, env = annotate_typ_entailment prog env ent in
        aents @ [ ent ], env)
      ~init:([], env) ents in
  ents, env
;;

(*******************************************************************
 ** find type substitution
 *******************************************************************)

let find_substitution_var sst expected_typ v : substitution =
  let typ = snd v in
  unify_typ sst typ expected_typ
;;

let rec find_substitution_exp env sst expected_typ e : substitution =
  try
    match e with
    | Int _ | Float _ | String _ -> sst
    | Var (v, _) -> find_substitution_var sst expected_typ v
    | BinExp (Add, e1, e2, l)
    | BinExp (Sub, e1, e2, l)
    | BinExp (Mul, e1, e2, l)
    | BinExp (Div, e1, e2, l) ->
      let sst = find_substitution_exp env sst (TInt 32) e1 in
      find_substitution_exp env sst (TInt 32) e2
    | Func (fname, es, t, l) ->
      (match env fname with
      | TFunc (arg_typs, ret_typ) ->
        let sst = unify_typ sst ret_typ t in
        List.fold2_exn ~f:(find_substitution_exp env) ~init:sst arg_typs es
      | t -> herror "find_substitution_exp: expect TFunc but found:" pr_type t)
  with exc -> error ("find_substitution_exp: " ^ pr_exp e ^ "\n")
;;

let rec find_substitution_form env sst f : substitution =
  try
    match f with
    | Bool _ | Emp _ -> sst
    | BVar (v, _) -> find_substitution_var sst TBool v
    | BinRel (Le, e1, e2, _)
    | BinRel (Lt, e1, e2, _)
    | BinRel (Ge, e1, e2, _)
    | BinRel (Gt, e1, e2, _) ->
      let sst = find_substitution_exp env sst (TInt 32) e1 in
      find_substitution_exp env sst (TInt 32) e2
    | BinRel (Eq, e1, e2, _) | BinRel (Ne, e1, e2, _) ->
      let tvar = fresh_tvar () in
      let sst = find_substitution_exp env sst tvar e1 in
      find_substitution_exp env sst tvar e2
    | Data (e, t, es, _, _) ->
      let sn = pr_type t in
      let res =
        match env sn with
        | TFunc (typs, _) ->
          List.fold2_exn ~f:(find_substitution_exp env) ~init:sst typs (e :: es)
        | t ->
          error
            ("find_subst_data_form " ^ pr_formula f ^ ": expect TFunc"
           ^ pr_type t) in
      res
    | Pred (pn, es, _) ->
      (match env pn with
      | TFunc (typs, _) ->
        List.fold2_exn ~f:(find_substitution_exp env) ~init:sst typs es
      | t ->
        error
          ("find_subst_pred_form " ^ pr_formula f
         ^ ": expect TFunc but found: " ^ pr_type t))
    | Array (root, size, etyp, _) ->
      let sst = find_substitution_exp env sst (TInt 32) size in
      find_substitution_exp env sst etyp root
    | BEq (e, f) ->
      let sst = find_substitution_exp env sst TBool e in
      find_substitution_form env sst f
    | Conj (f1, f2)
    | Disj (f1, f2)
    | Star (f1, f2)
    | Wand (f1, f2)
    | Septract (f1, f2)
    | Update (f1, f2) ->
      let sst = find_substitution_form env sst f1 in
      find_substitution_form env sst f2
    | Neg g | Forall (_, g) | Exists (_, g) -> find_substitution_form env sst g
  with e -> error ("find_substitution_form: " ^ pr_formula f ^ "\n")
;;

let find_substitution_forms env sst fs : substitution =
  List.fold_left ~f:(find_substitution_form env) ~init:sst fs
;;

let find_substitution_entail env sst ent : substitution =
  let lhs, rhs = ent.ent_lhs, ent.ent_rhs in
  let sst = find_substitution_form env sst lhs in
  find_substitution_form env sst rhs
;;

let find_substitution_expntails env sst ents : substitution =
  List.fold_left ~f:(find_substitution_entail env) ~init:sst ents
;;

(*******************************************************************
 ** apply type substitution
 *******************************************************************)

let substitute_var (sst : substitution) (v : var) : var =
  let vname, typ = v in
  vname, sst typ
;;

let substitute_vars sst vs : var list = List.map ~f:(substitute_var sst) vs

let rec substitute_exp sst e : exp =
  match e with
  | Int _ | Float _ | String _ -> e
  | Var (v, l) -> Var (substitute_var sst v, l)
  | BinExp (op, e1, e2, l) ->
    let e1, e2 = substitute_exp sst e1, substitute_exp sst e2 in
    BinExp (op, e1, e2, l)
  | Func (fn, es, t, l) -> Func (fn, substitute_exps sst es, sst t, l)

and substitute_exps sst es = List.map ~f:(substitute_exp sst) es

let substitute_addr_exp sst a : addr_exp =
  { addr_base = substitute_exp sst a.addr_base;
    addr_elem = substitute_exp sst a.addr_elem;
    addr_field = substitute_exp sst a.addr_field
  }
;;

let rec substitute_form sst f : formula =
  match f with
  | Bool _ | Emp _ -> f
  | BVar (v, l) -> BVar (substitute_var sst v, l)
  | BinRel (op, e1, e2, l) ->
    BinRel (op, substitute_exp sst e1, substitute_exp sst e2, l)
  | Data (e, dn, es, addr, l) ->
    let addr = Option.map ~f:(substitute_addr_exp sst) addr in
    Data (substitute_exp sst e, dn, substitute_exps sst es, addr, l)
  | Pred (pn, es, l) -> Pred (pn, substitute_exps sst es, l)
  | Array (root, size, etyp, l) ->
    Array (substitute_exp sst root, substitute_exp sst size, etyp, l)
  | Neg g -> Neg (substitute_form sst g)
  | BEq (e, f) -> BEq (substitute_exp sst e, substitute_form sst f)
  | Conj (f1, f2) -> Conj (substitute_form sst f1, substitute_form sst f2)
  | Disj (f1, f2) -> Disj (substitute_form sst f1, substitute_form sst f2)
  | Star (f1, f2) -> Star (substitute_form sst f1, substitute_form sst f2)
  | Wand (f1, f2) -> Wand (substitute_form sst f1, substitute_form sst f2)
  | Septract (f1, f2) ->
    Septract (substitute_form sst f1, substitute_form sst f2)
  | Update (f1, f2) -> Update (substitute_form sst f1, substitute_form sst f2)
  | Forall (vs, g) -> Forall (substitute_vars sst vs, substitute_form sst g)
  | Exists (vs, g) -> Exists (substitute_vars sst vs, substitute_form sst g)
;;

let substitute_forms sst fs = List.map ~f:(substitute_form sst) fs

let substitute_entail sst ent : entailment =
  let lhs, rhs = ent.ent_lhs, ent.ent_rhs in
  { ent with
    ent_lhs = substitute_form sst lhs;
    ent_rhs = substitute_form sst rhs
  }
;;

let substitute_entails sst ents : entailment list =
  List.map ~f:(substitute_entail sst) ents
;;

let substitute_func_defn sst fdefn : func_defn =
  match fdefn.funcd_body with
  | None ->
    { fdefn with funcd_params = substitute_vars sst fdefn.funcd_params }
  | Some e ->
    let params = substitute_vars sst fdefn.funcd_params in
    let body = Some (substitute_exp sst e) in
    { fdefn with funcd_params = params; funcd_body = body }
;;

let substitute_pred_defn sst pdefn : pred_defn =
  let params = List.map ~f:(substitute_var sst) pdefn.predd_params in
  let body = substitute_forms sst pdefn.predd_body in
  { pdefn with predd_params = params; predd_body = body }
;;

let substitute_command sst cmd : command =
  match cmd with
  | CheckSat (f, l) -> CheckSat (substitute_form sst f, l)
  | ProveEntails (ents, l) -> ProveEntails (substitute_entails sst ents, l)
  | InferFrame (ent, l) -> InferFrame (substitute_entail sst ent, l)
;;

let substitute_program prog sst : program =
  let fdefns = List.map ~f:(substitute_func_defn sst) prog.prog_func_defns in
  let pdefns = List.map ~f:(substitute_pred_defn sst) prog.prog_pred_defns in
  let commands = List.map ~f:(substitute_command sst) prog.prog_commands in
  { prog with
    prog_func_defns = fdefns;
    prog_pred_defns = pdefns;
    prog_commands = commands
  }
;;

(*******************************************************************
 ** infer type
 *******************************************************************)

let infer_typ_data_defn prog env sst ddefn
    : data_defn * substitution * environment
  =
  let root_typ = ddefn.datad_typ in
  let field_typs = ddefn.datad_fields |> List.unzip |> fst in
  let dtyp = TFunc (root_typ :: field_typs, TBool) in
  let dname = pr_type ddefn.datad_typ in
  let env = extend_env env dname dtyp in
  ddefn, sst, env
;;

let infer_typ_func_defn prog env sst fdefn
    : func_defn * substitution * environment
  =
  let fdefn, sst =
    match fdefn.funcd_body with
    | None ->
      let params, _ = annotate_typ_vars prog env fdefn.funcd_params in
      let fdefn = { fdefn with funcd_params = params } in
      fdefn, sst
    | Some e ->
      let e, _ = annotate_typ_exp prog env e in
      let sst = find_substitution_exp env sst fdefn.funcd_ret_typ e in
      let params = List.map ~f:(substitute_var sst) fdefn.funcd_params in
      let e = substitute_exp sst e in
      let fdefn = { fdefn with funcd_params = params; funcd_body = Some e } in
      fdefn, sst in
  let ftyp =
    let param_typs = List.map ~f:typ_of_var fdefn.funcd_params in
    let ret_type =
      match fdefn.funcd_body with
      | None -> fresh_tvar ()
      | Some e -> typ_of_exp e in
    TFunc (param_typs, ret_type) in
  let env = extend_env env fdefn.funcd_name ftyp in
  fdefn, sst, env
;;

let infer_typ_pred_defn prog env sst pdefn
    : pred_defn * substitution * environment
  =
  let pdefn, sst =
    let fs, env = annotate_typ_formulas prog env pdefn.predd_body in
    let params, env = annotate_typ_vars prog env pdefn.predd_params in
    let sst = find_substitution_forms env sst fs in
    let params = substitute_vars sst params in
    let body = substitute_forms sst fs in
    let pdefn = { pdefn with predd_params = params; predd_body = body } in
    pdefn, sst in
  let param_typs = List.map ~f:typ_of_var pdefn.predd_params in
  let ptyp = TFunc (param_typs, TBool) in
  let env = extend_env env pdefn.predd_name ptyp in
  pdefn, sst, env
;;

let infer_typ_command prog env sst cmd : command * substitution * environment =
  match cmd with
  | CheckSat (f, l) ->
    let f, env = annotate_typ_formula prog env f in
    let sst = find_substitution_form env sst f in
    let f = substitute_form sst f in
    let cmd = CheckSat (f, l) in
    cmd, sst, env
  | ProveEntails (ents, l) ->
    let ents, env = annotate_typ_entailments prog env ents in
    let sst = find_substitution_expntails env sst ents in
    let ents = substitute_entails sst ents in
    let cmd = ProveEntails (ents, l) in
    cmd, sst, env
  | InferFrame (ent, l) ->
    let ent, env = annotate_typ_entailment prog env ent in
    let sst = find_substitution_entail env sst ent in
    let ent = substitute_entail sst ent in
    let cmd = InferFrame (ent, l) in
    cmd, sst, env
;;

(*******************************************************************
 ** infer type
 *******************************************************************)

let infer_typ_program prog : program =
  let env = new_typ_env () in
  let sst = mk_subst_id () in
  let ddefns, sst, env =
    List.fold_left
      ~f:(fun (dds, sst, env) dd ->
        let dd, sst, env = infer_typ_data_defn prog env sst dd in
        dds @ [ dd ], sst, env)
      ~init:([], sst, env) prog.prog_data_defns in
  let fdefns, sst, env =
    List.fold_left
      ~f:(fun (fds, sst, env) fd ->
        let fd, sst, env = infer_typ_func_defn prog env sst fd in
        fds @ [ fd ], sst, env)
      ~init:([], sst, env) prog.prog_func_defns in
  let pdefns, sst, env =
    List.fold_left
      ~f:(fun (pds, sst, env) pd ->
        let pd, sst, env = infer_typ_pred_defn prog env sst pd in
        pds @ [ pd ], sst, env)
      ~init:([], sst, env) prog.prog_pred_defns in
  let cmds, sst, env =
    List.fold_left
      ~f:(fun (cmds, sst, env) cmd ->
        let cmd, sst, env = infer_typ_command prog env sst cmd in
        cmds @ [ cmd ], sst, env)
      ~init:([], sst, env) prog.prog_commands in
  let prog =
    { prog with
      prog_data_defns = ddefns;
      prog_func_defns = fdefns;
      prog_pred_defns = pdefns;
      prog_commands = cmds
    } in
  (* finally apply the substitution again *)
  substitute_program prog sst
;;
