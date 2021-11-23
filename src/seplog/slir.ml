(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Libdiscover
open Debugger
module SA = Slast
module LL = Llvm
module LI = Llir
module SP = Set.Poly
include Slvar

(*******************************************************************
 ** constraints
 *******************************************************************)

type func_symbol =
  | Add
  | Sub
  | Mul
  | Div (* binary functions *)
  | FName of string
[@@deriving equal]

type func_exp = { func_name : func_symbol }

type exp =
  | Void
  | Null
  | Int of int
  | Float of float
  | String of string
  | Var of var
  | Cast of (exp * typ * typ)
  | Func of (func_symbol * exp list * typ)
[@@deriving equal]

type addr_exp =
  { addr_base : exp;
    addr_elem : exp;
    addr_field : exp
  }

type exps = exp list

(*** first-order logic formulas ***)

type relation_symbol =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge (* binary relations *)
  | RName of string

type reln_form = relation_symbol * exp list

type pure_form =
  | Bool of bool
  | BExp of exp
  | Reln of reln_form
  | PNeg of pure_form
  | BEq of (exp * pure_form)
  | PConj of pure_form list
  | PDisj of pure_form list
  | PForall of (var list * pure_form)
  | PExists of (var list * pure_form)

type pure_forms = pure_form list

(*** basic data structure ***)

type data_form =
  { data_typ : typ;
    data_root : exp;
    data_args : exp list;
    data_addr : addr_exp option
  }

type view_form =
  { view_name : string;
    view_args : exp list
  }

type iter_form =
  { (* iterated separating conjunction *)
    iter_base : exp;
    iter_element_index : exp;
    iter_running_index : var;
    iter_begin : exp;
    iter_end : exp
        (* iter_content : ?: *)
        (* TODO: how to model content? *)
  }

type array_form =
  { array_root : exp;
    array_size : exp;
    array_typ : typ;
    array_update : data_form list;
    (* update is always applied first *)
    array_subtract : data_form list
  }

(*** formulas ***)

type formula =
  | Emp
  | Pure of pure_form
  | Data of data_form
  | View of view_form
  | Iter of iter_form
  | Array of array_form
  | Star of formula list
  | Wand of (formula * formula)
  | Septract of (formula * formula)
  | Exists of (var list * formula)

(*
   TODO:
   - normalize update to attach to atomic formula like Data/View/Array
   - when inferring frame, use Septract to represent the frame
*)

type formulas = formula list

(*** entailments ***)

type entailment =
  { ent_id : int;
    ent_rhs : formula;
    ent_lhs : formula
  }

type entailments = entailment list

(*** other types ***)

type smt_result = bool option * (var * exp) list

(*******************************************************************
 ** definitions
 *******************************************************************)

type func_defn =
  { funcd_name : string;
    funcd_params : var list;
    funcd_body : exp option;
    funcd_ret_typ : typ
  }

type reln_defn =
  { relnd_name : string;
    relnd_params : var list;
    relnd_body : pure_form option
  }

type data_defn =
  { datad_typ : typ;
    datad_fields : (typ * string) list
  }

type view_defn_case =
  { vdc_id : int;
    vdc_form : formula;
    vdc_is_base_case : bool
  }

type view_defn =
  { viewd_name : string;
    viewd_params : var list;
    viewd_body : view_defn_case list
  }

type proc_specs =
  { psp_precond : formula;
    psp_postcond : formula
  }

type proc_defn =
  { procd_name : string;
    procd_params : var list;
    procd_return_typ : typ;
    procd_specs : proc_specs list
  }

type command =
  | CheckSat of formula
  | ProveEntails of entailments
  | InferFrame of entailment

type program =
  { prog_file_name : string;
    prog_func_defns : func_defn list;
    prog_reln_defns : reln_defn list;
    prog_data_defns : data_defn list;
    prog_view_defns : view_defn list;
    prog_proc_defns : proc_defn list;
    prog_commands : command list
  }

(*******************************************************************
 ** printing
 *******************************************************************)

let rec pr_exp (e : exp) : string =
  match e with
  | Void -> "void"
  | Null -> "null"
  | Int i -> pr_int i
  | Float f -> pr_float f
  | String s -> "\"" ^ s ^ "\""
  | Var v -> pr_var v
  | Cast (e, t1, t2) ->
    "cast(" ^ pr_exp e ^ "," ^ pr_type t1 ^ "," ^ pr_type t2 ^ ")"
  | Func (Add, [ e1; e2 ], _) -> pr_exp e1 ^ "+" ^ pr_exp e2
  | Func (Sub, [ e1; e2 ], _) -> pr_exp e1 ^ "-" ^ pr_exp e2
  | Func (Mul, [ e1; e2 ], _) -> pr_exp e1 ^ "*" ^ pr_exp e2
  | Func (Div, [ e1; e2 ], _) -> pr_exp e1 ^ "/" ^ pr_exp e2
  | Func (FName n, es, _) -> n ^ "(" ^ pr_exps es ^ ")"
  | Func _ -> error "pr_exp: unexpected exp: (need details)"

and pr_exps (es : exp list) : string =
  pr_list ~sep:"," ~f:pr_exp es
;;

let pr_reln_form (rf : reln_form) : string =
  match rf with
  | Eq, [ e1; e2 ] -> pr_exp e1 ^ "=" ^ pr_exp e2
  | Ne, [ e1; e2 ] -> pr_exp e1 ^ "!=" ^ pr_exp e2
  | Lt, [ e1; e2 ] -> pr_exp e1 ^ "<" ^ pr_exp e2
  | Gt, [ e1; e2 ] -> pr_exp e1 ^ ">" ^ pr_exp e2
  | Le, [ e1; e2 ] -> pr_exp e1 ^ "<=" ^ pr_exp e2
  | Ge, [ e1; e2 ] -> pr_exp e1 ^ ">=" ^ pr_exp e2
  | RName n, es -> n ^ "(" ^ pr_exps es ^ ")"
  | _ -> error "pr_reln_form: unexpected relation: (need details)"
;;

let rec pr_pure_form (f : pure_form) : string =
  let pr_aux f =
    match f with
    | PConj _ | PDisj _ -> "(" ^ pr_pure_form f ^ ")"
    | _ -> pr_pure_form f in
  match f with
  | Bool b -> pr_bool b
  | BExp e -> pr_exp e
  | Reln rf -> pr_reln_form rf
  | PNeg f -> "!" ^ "(" ^ pr_pure_form f ^ ")"
  | BEq (e, p) -> pr_exp e ^ "=" ^ "(" ^ pr_pure_form p ^ ")"
  | PConj fs -> pr_list ~sep:" & " ~f:pr_aux fs
  | PDisj fs -> pr_list ~sep:" | " ~f:pr_aux fs
  | PForall (vs, f) ->
    "(forall " ^ pr_vars vs ^ ". " ^ pr_pure_form f ^ ")"
  | PExists (vs, f) ->
    "(exists " ^ pr_vars vs ^ ". " ^ pr_pure_form f ^ ")"

and pr_pf (f : pure_form) = pr_pure_form f

and pr_pfs (fs : pure_forms) : string =
  pr_list ~sep:"\n" ~f:pr_pf fs
;;

let pr_addr_form a =
  "(" ^ pr_exp a.addr_base ^ "," ^ pr_exp a.addr_elem ^ ")"
;;

let pr_data_form (d : data_form) : string =
  let addr =
    match d.data_addr with
    | None -> ""
    | Some addr -> "@" ^ pr_addr_form addr in
  pr_exp d.data_root
  ^ "->"
  ^ pr_type d.data_typ
  ^ "{"
  ^ pr_exps d.data_args
  ^ "}"
  ^ addr
;;

let pr_df (s : data_form) = pr_data_form s

let pr_dfs sfs : string =
  pr_list ~obrace:"[" ~cbrace:"]" ~sep:", " ~f:pr_df sfs
;;

let pr_view_form (v : view_form) : string =
  v.view_name ^ "(" ^ pr_exps v.view_args ^ ")"
;;

let pr_vf (v : view_form) = pr_view_form v

let pr_vfs vfs : string =
  pr_list ~obrace:"[" ~cbrace:"]" ~sep:", " ~f:pr_vf vfs
;;

let pr_iter_form (i : iter_form) =
  "aiter("
  ^ pr_exp i.iter_base
  ^ ","
  ^ pr_exp i.iter_element_index
  ^ ")"
  ^ "{"
  ^ pr_var i.iter_running_index
  ^ ","
  ^ pr_exp i.iter_begin
  ^ ","
  ^ pr_exp i.iter_end
  ^ "}"
;;

let pr_if (i : iter_form) = pr_iter_form i

let pr_array_form (a : array_form) =
  let res =
    "array("
    ^ pr_exp a.array_root
    ^ ","
    ^ pr_exp a.array_size
    ^ ","
    ^ pr_type a.array_typ
    ^ ")" in
  let res =
    List.fold_left
      ~f:(fun acc d -> "(" ^ acc ^ " *+ " ^ pr_df d ^ ")")
      ~init:res
      a.array_update in
  let res =
    List.fold_left
      ~f:(fun acc d -> "(" ^ acc ^ " *- " ^ pr_df d ^ ")")
      ~init:res
      a.array_subtract in
  res
;;

let pr_af (a : array_form) = pr_array_form a

let pr_formula (r : formula) : string =
  let rec pr_core r =
    match r with
    | Emp -> "emp"
    | Pure p -> pr_pf p
    | Data s -> pr_df s
    | View v -> pr_vf v
    | Iter i -> pr_if i
    | Array a -> pr_af a
    | Star fs -> fs |> List.map ~f:pr_core |> String.concat ~sep:" * "
    | Wand (f1, f2) -> pr_core f1 ^ " --* " ^ pr_aux f2
    | Septract (f1, f2) -> pr_aux f1 ^ " *- " ^ pr_aux f2
    | Exists (vs, g) -> "(exists " ^ pr_vars vs ^ ". " ^ pr_core g ^ ")"
  and pr_aux f =
    match f with
    | Emp | Data _ | View _ | Array _ | Iter _ -> pr_core f
    | Pure (Bool _) | Pure (BExp _) | Pure (Reln _) -> pr_core f
    | _ -> "(" ^ pr_core f ^ ")" in
  pr_core r
;;

let pr_f (f : formula) = pr_formula f

let pr_fs ?(sep = ", ") (fs : formulas) : string =
  pr_list ~obrace:"[" ~cbrace:"]" ~sep ~f:pr_f fs
;;

let pr_ent ?(id = false) ent =
  let res = pr_f ent.ent_lhs ^ " |- " ^ pr_f ent.ent_rhs in
  if (not id) || ent.ent_id < 1
  then "# " ^ res
  else "#" ^ pr_int ent.ent_id ^ ". " ^ res
;;

let pr_ents (ents : entailments) : string =
  hpr_list_itemized ~bullet:"  " ~f:(pr_ent ~id:true) ents
;;

let pr_ent_id (ent : entailment) : string = "#" ^ pr_int ent.ent_id

let pr_ents_ids (ents : entailments) : string =
  ents |> List.map ~f:pr_ent_id |> String.concat ~sep:", "
;;

(*** print declarations ***)

let pr_func_defn (f : func_defn) : string =
  let header =
    "func " ^ f.funcd_name ^ "(" ^ pr_vars f.funcd_params ^ ") := " in
  let body =
    match f.funcd_body with
    | None -> "?"
    | Some e -> pr_exp e in
  header ^ body ^ ";"
;;

let pr_reln_defn (r : reln_defn) : string =
  let header =
    "rel " ^ r.relnd_name ^ "(" ^ pr_vars r.relnd_params ^ ") := " in
  let body =
    match r.relnd_body with
    | None -> "?"
    | Some f -> pr_pf f in
  header ^ body ^ ";"
;;

let pr_data_defn (d : data_defn) : string =
  let fields =
    d.datad_fields
    |> List.map ~f:(fun (t, n) -> "  " ^ pr_type t ^ " " ^ n ^ ";")
    |> String.concat ~sep:"\n" in
  "data " ^ pr_type d.datad_typ ^ "{\n" ^ fields ^ "\n};"
;;

let pr_view_defn_case (vdc : view_defn_case) : string =
  pr_f vdc.vdc_form
;;

let pr_view_defn (v : view_defn) : string =
  let header =
    "view " ^ v.viewd_name ^ "(" ^ pr_vars v.viewd_params ^ ") := " in
  let body =
    match v.viewd_body with
    | [] -> "?"
    | fs -> pr_list ~sep:"\n    \\/ " ~f:pr_view_defn_case fs in
  header ^ body ^ ";"
;;

let pr_command (c : command) =
  match c with
  | CheckSat f -> "CheckSat: " ^ pr_f f ^ ";"
  | ProveEntails ents -> "ProveEntails:\n" ^ pr_ents ents ^ ";"
  | InferFrame ent -> "InferFrame: " ^ pr_ent ent ^ ";"
;;

let pr_program (p : program) : string =
  let datas = p.prog_data_defns |> List.map ~f:pr_data_defn in
  let relns = p.prog_reln_defns |> List.map ~f:pr_reln_defn in
  let views = p.prog_view_defns |> List.map ~f:pr_view_defn in
  let funcs = p.prog_func_defns |> List.map ~f:pr_func_defn in
  let cmds = p.prog_commands |> List.map ~f:pr_command in
  String.concat ~sep:"\n\n" (funcs @ datas @ relns @ views @ funcs @ cmds)
;;

(*******************************************************************
 ** comparison
 *******************************************************************)

let mem_exp e es = List.exists ~f:(equal_exp e) es
let equal_df d1 d2 = String.equal (pr_df d1) (pr_df d2)
let equal_vf v1 v2 = String.equal (pr_vf v1) (pr_vf v2)

let equal_df_form f1 f2 =
  match f1, f2 with
  | Data d1, Data d2 -> equal_df d1 d2
  | _ -> false
;;

let equal_df_form_name_root f1 f2 =
  match f1, f2 with
  | Data d1, Data d2 ->
    equal_typ d1.data_typ d2.data_typ && equal_exp d1.data_root d2.data_root
  | _ -> false
;;

let equal_vf_form f1 f2 =
  match f1, f2 with
  | View v1, View v2 -> equal_vf v1 v2
  | _ -> false
;;

let compare_f f1 f2 =
  let encode f =
    match f with
    | Pure _ -> 0
    | Emp -> 1
    | Data _ -> 2
    | View _ -> 3
    | Array _ -> 4
    | _ -> 5 in
  let cmp = encode f1 - encode f2 in
  if cmp != 0 then cmp else String.compare (pr_f f1) (pr_f f2)
;;

(*******************************************************************
 ** type queries
 *******************************************************************)

let typ_of_exp e =
  match e with
  | Void _ -> TVoid
  | Null _ -> TPointer TVoid
  | Int _ -> TInt 32
  | Float _ -> TFloat
  | String _ -> TString
  | Var v -> typ_of_var v
  | Cast (_, _, t) -> t
  | Func (_, _, t) -> t
;;

let get_pointer_elem_typ t =
  match t with
  | TPointer elem_typ -> elem_typ
  | _ -> herror "get_elem_typ: not a pointer type" pr_type t
;;

(*******************************************************************
 ** variable queries
 *******************************************************************)

let fv_exp (e : exp) : var list =
  let rec fv acc e =
    match e with
    | Void _ | Null _ | Int _ | Float _ | String _ -> acc
    | Var v -> insert_vs v acc
    | Cast (e, _, _) -> fv acc e
    | Func (_, es, _) -> List.fold_left ~f:fv ~init:acc es in
  fv [] e
;;

let fv_e = fv_exp
let fv_es (es : exps) : var list = es |> List.map ~f:fv_e |> merge_vs
let fv_reln_form (rf : reln_form) = fv_es (snd rf)
let fv_rf = fv_reln_form

let fv_pure_form (p : pure_form) : var list =
  let rec fv acc p =
    match p with
    | Bool _ -> acc
    | BExp e -> merge_vs [ acc; fv_e e ]
    | BEq (e, p) -> merge_vs [ fv_e e; fv acc p ]
    | Reln rf -> merge_vs [ acc; fv_rf rf ]
    | PNeg f -> fv acc f
    | PConj fs | PDisj fs -> List.fold_left ~f:fv ~init:acc fs
    | PForall (vs, f) | PExists (vs, f) -> diff_vs (fv acc f) vs in
  fv [] p
;;

let fv_pf = fv_pure_form
let fv_pfs (ps : pure_forms) : var list = ps |> List.map ~f:fv_pf |> merge_vs
let fv_data_form (d : data_form) : var list = fv_es (d.data_root :: d.data_args)
let fv_df = fv_data_form

let fv_dfs (ds : data_form list) : var list =
  ds |> List.map ~f:fv_df |> merge_vs
;;

let fv_view_form (v : view_form) : var list = fv_es v.view_args
let fv_vf = fv_view_form

let fv_vfs (vs : view_form list) : var list =
  vs |> List.map ~f:fv_vf |> merge_vs
;;

let fv_iter i =
  let index_vs = diff_vs (fv_e i.iter_element_index) [ i.iter_running_index ] in
  merge_vs [ fv_e i.iter_base; fv_e i.iter_begin; fv_e i.iter_end; index_vs ]
;;

let fv_if = fv_iter

let fv_array_form (a : array_form) : var list =
  fv_es [ a.array_root; a.array_size ]
;;

let fv_af = fv_array_form

let fv_afs (afs : array_form list) : var list =
  afs |> List.map ~f:fv_af |> merge_vs
;;

let fv_form (f : formula) : var list =
  let rec fv f =
    match f with
    | Emp -> []
    | Pure p -> fv_pf p
    | Data d -> fv_df d
    | View v -> fv_vf v
    | Iter i -> fv_if i
    | Array a -> fv_af a
    | Star fs -> fs |> List.map ~f:fv |> merge_vs
    | Wand (f1, f2) | Septract (f1, f2) -> merge_vs [ fv f1; fv f2 ]
    | Exists (vs, f) -> diff_vs (fv f) vs in
  fv f
;;

let fv_f = fv_form
let fv_fs (fs : formulas) : var list = fs |> List.map ~f:fv_f |> merge_vs

let fv_spec (spec : proc_specs) : var list =
  fv_fs [ spec.psp_precond; spec.psp_postcond ]
;;

let fv_specs (specs : proc_specs list) : var list =
  specs |> List.map ~f:fv_spec |> merge_vs
;;

(*******************************************************************
 ** simple queries
 *******************************************************************)

let is_int_exp e =
  match typ_of_exp e with
  | TInt _ -> true
  | _ -> false
;;

let is_expr_null e =
  match e with
  | Null -> true
  | _ -> false
;;

let is_exp_var e =
  match e with
  | Var v -> true
  | _ -> false
;;

let is_pf_true p =
  match p with
  | Bool true -> true
  | _ -> false
;;

let is_pf_false p =
  match p with
  | Bool false -> true
  | _ -> false
;;

let is_pf_exists p =
  match p with
  | PExists _ -> true
  | _ -> false
;;

let is_pf_forall p =
  match p with
  | PForall _ -> true
  | _ -> false
;;

let rec is_f_false p =
  match p with
  | Pure p -> is_pf_false p
  | Star fs -> List.exists ~f:is_f_false fs
  | _ -> false
;;

let is_f_true f =
  match f with
  | Pure p -> is_pf_true p
  | _ -> false
;;

let is_f_exists f =
  match f with
  | Exists _ -> true
  | _ -> false
;;

let rec is_f_pure f =
  match f with
  | Pure _ | Emp _ -> true
  | Data _ | View _ | Iter _ | Array _ -> false
  | Star fs -> List.for_all ~f:is_f_pure fs
  | Wand (f1, f2) -> is_f_pure f1 && is_f_pure f2
  | Septract (f1, f2) -> is_f_pure f1 && is_f_pure f2
  | Exists (_, g) -> is_f_pure g
;;

let is_f_emp f =
  match f with
  | Emp -> true
  | _ -> false
;;

let is_f_data (f : formula) =
  match f with
  | Data _ -> true
  | _ -> false
;;

let is_f_view (f : formula) =
  match f with
  | View _ -> true
  | _ -> false
;;

let is_f_array (f : formula) =
  match f with
  | Array _ -> true
  | _ -> false
;;

let is_f_data_or_view (f : formula) =
  match f with
  | Data _ -> true
  | View _ -> true
  | _ -> false
;;

let rec has_f_wand (f : formula) =
  match f with
  | Wand _ -> true
  | Star fs -> List.exists ~f:has_f_wand fs
  | Exists (_, f) -> has_f_wand f
  | _ -> false
;;

let rec has_f_qvars (f : formula) =
  match f with
  | Exists _ -> true
  | Wand (f1, f2) -> has_f_qvars f1 || has_f_qvars f2
  | Star fs -> List.exists ~f:has_f_qvars fs
  | _ -> false
;;

(*******************************************************************
 ** constructors
 *******************************************************************)

let mk_exp_int i = Int i
let mk_exp_float f = Float f
let mk_exp_string s = String s
let mk_exp_var v = Var v

let fresh_exp_var ?(name = "t") (typ : typ) =
  mk_exp_var (fresh_new_var ~name typ)
;;

let mk_void () = Void
let mk_null () = Null
let mk_exp_var_typ vname typ = Var (mk_var vname typ)
let fresh_exp_var ?(name = "t") typ = Var (fresh_new_var ~name typ)
let mk_cast_exp e old_typ new_typ = Cast (e, old_typ, new_typ)

let mk_add e1 e2 =
  let t1, t2 = typ_of_exp e1, typ_of_exp e2 in
  let typ =
    match t1, t2 with
    | TFloat, _ | _, TFloat -> TFloat
    | TInt i1, TInt i2 -> TInt (max i1 i2)
    | _ -> error "mk_add: expect int or float type" in
  Func (Add, [ e1; e2 ], typ)
;;

let mk_sub e1 e2 =
  let t1, t2 = typ_of_exp e1, typ_of_exp e2 in
  let typ =
    match t1, t2 with
    | TFloat, _ | _, TFloat -> TFloat
    | TInt i1, TInt i2 -> TInt (max i1 i2)
    | _ ->
      error
        ("mk_sub "
        ^ pr_exp e1
        ^ " and "
        ^ pr_exp e2
        ^ ": expect int or float but found") in
  Func (Sub, [ e1; e2 ], typ)
;;

let mk_mul e1 e2 =
  let t1, t2 = typ_of_exp e1, typ_of_exp e2 in
  let typ =
    match t1, t2 with
    | TFloat, _ | _, TFloat -> TFloat
    | TInt i1, TInt i2 -> TInt (max i1 i2)
    | _ -> error "mk_mul: expect int or float type" in
  Func (Mul, [ e1; e2 ], typ)
;;

let mk_mul_exp_int e i = mk_mul e (mk_exp_int i)

let mk_div e1 e2 =
  let t1, t2 = typ_of_exp e1, typ_of_exp e2 in
  let typ =
    match t1, t2 with
    | TFloat, _ | _, TFloat -> TFloat
    | TInt i1, TInt i2 -> TInt (max i1 i2)
    | _ -> error "mk_div: expect int or float type" in
  Func (Div, [ e1; e2 ], typ)
;;

let mk_func ?(typ = TUnk) fn args = Func (FName fn, args, typ)

(* atoms *)

let mk_bool b = Bool b
let mk_bexp e = BExp e
let mk_beq e p = BEq (e, p)
let mk_eq e1 e2 = Reln (Eq, [ e1; e2 ])
let mk_eq_exp_int e i = Reln (Eq, [ e; mk_exp_int i ])
let mk_ne e1 e2 = Reln (Ne, [ e1; e2 ])
let mk_lt e1 e2 = Reln (Lt, [ e1; e2 ])
let mk_gt e1 e2 = Reln (Gt, [ e1; e2 ])
let mk_le e1 e2 = Reln (Le, [ e1; e2 ])
let mk_ge e1 e2 = Reln (Ge, [ e1; e2 ])
let mk_reln rel args = rel, args
let mk_preln r args = Reln (r, args)

let mk_ne_exps exps : pure_form =
  let rec mk_aux es =
    match es with
    | [] -> []
    | e :: es ->
      let pfs1 = mk_aux es in
      let pfs2 = List.map ~f:(fun a -> mk_ne e a) es in
      pfs1 @ pfs2 in
  let pfs = mk_aux exps in
  PConj pfs
;;

(* pure formulas *)

let mk_pf_true () = mk_bool true
let mk_pf_false () = mk_bool false

let mk_pneg f =
  match f with
  | Bool b -> if b then Bool false else Bool true
  | Reln (Eq, [ e1; e2 ]) -> Reln (Ne, [ e1; e2 ])
  | Reln (Ne, [ e1; e2 ]) -> Reln (Eq, [ e1; e2 ])
  | Reln (Lt, [ e1; e2 ]) -> Reln (Ge, [ e1; e2 ])
  | Reln (Le, [ e1; e2 ]) -> Reln (Gt, [ e1; e2 ])
  | Reln (Gt, [ e1; e2 ]) -> Reln (Le, [ e1; e2 ])
  | Reln (Ge, [ e1; e2 ]) -> Reln (Lt, [ e1; e2 ])
  | _ -> PNeg f
;;

let mk_pconj fs =
  let rec flatten acc fs =
    match fs with
    | [] -> acc
    | PConj gs :: nfs -> flatten (flatten acc gs) nfs
    | f :: nfs -> flatten (acc @ [ f ]) nfs in
  let nfs = fs |> flatten [] |> List.exclude ~f:is_pf_true in
  match nfs with
  | [] -> mk_pf_true ()
  | [ nf ] -> nf
  | _ -> PConj nfs
;;

let mk_pdisj fs =
  let rec flatten acc fs =
    match fs with
    | [] -> acc
    | PDisj gs :: nfs -> flatten (flatten acc gs) nfs
    | f :: nfs -> flatten (acc @ [ f ]) nfs in
  let nfs = fs |> flatten [] |> List.exclude ~f:is_pf_false in
  match nfs with
  | [] -> mk_pf_false ()
  | [ nf ] -> nf
  | _ -> PDisj nfs
;;

let mk_pforall vs f =
  let vs = intersect_vs vs (fv_pf f) in
  if List.is_empty vs then f else PForall (vs, f)
;;

let mk_pexists vs f =
  let vs = intersect_vs vs (fv_pf f) in
  if List.is_empty vs then f else PExists (vs, f)
;;

(* data structures *)

let mk_addr base elem field =
  { addr_base = base; addr_elem = elem; addr_field = field }
;;

let mk_addr_base base = mk_addr base (mk_exp_int 0) (mk_exp_int 0)

let mk_data ?(addr = None) typ root args =
  { data_typ = typ; data_root = root; data_args = args; data_addr = addr }
;;

let mk_view name args = { view_name = name; view_args = args }

(* array *)

let mk_iter base index iter begin_iter end_iter =
  { iter_base = base;
    iter_element_index = index;
    iter_running_index = iter;
    iter_begin = begin_iter;
    iter_end = end_iter
  }
;;

let mk_array ?(update = []) ?(subtract = []) root size etyp =
  { array_root = root;
    array_size = size;
    array_typ = etyp;
    array_update = update;
    array_subtract = subtract
  }
;;

(* formulas *)

let f_of_pure p = Pure p
let f_of_data s = Data s
let f_of_view v = View v
let f_of_iter i = Iter i
let f_of_array a = Array a
let mk_emp () = Emp
let mk_f_pure p = Pure p
let mk_f_false () = Pure (mk_pf_false ())
let mk_f_true () = Pure (mk_pf_true ())
let mk_f_data ?(addr = None) typ root args = Data (mk_data ~addr typ root args)
let mk_f_view name args = View (mk_view name args)

let mk_f_iter base index iter begin_iter end_iter =
  Iter (mk_iter base index iter begin_iter end_iter)
;;

let mk_f_array a = Array a
let mk_f_arrays afs = Star (List.map ~f:mk_f_array afs)

let mk_f_star fs =
  let rec flatten acc gs =
    match gs with
    | [] -> acc
    | Star ks :: ngs -> flatten (flatten acc ks) ngs
    | g :: ngs -> flatten (acc @ [ g ]) ngs in
  let fs = flatten [] fs in
  let fs, ps =
    List.fold_left
      ~f:(fun (afs, aps) f ->
        match f with
        | Emp -> afs, aps
        | Pure p -> afs, aps @ [ p ]
        | _ -> afs @ [ f ], aps)
      ~init:([], [])
      fs in
  let fs =
    match ps with
    | [] -> fs
    | _ -> fs @ [ Pure (mk_pconj ps) ] in
  let fs = List.sort ~compare:compare_f fs in
  match fs with
  | [] -> Emp
  | [ f ] -> f
  | _ -> Star fs
;;

let mk_f_wand f1 f2 = Wand (f1, f2)
let mk_f_septract f1 f2 = Septract (f1, f2)

let mk_f_exists vs f =
  let rec make vs f =
    match vs, f with
    | [], _ -> f
    | _, Exists (nvs, f) -> make (merge_vs [ vs; nvs ]) f
    | _, _ -> Exists (vs, f) in
  let nvs = intersect_vs vs (fv_f f) in
  make nvs f
;;

(*** definitions ***)

let mk_entailment ?(id = 0) lhs rhs =
  { ent_id = id; ent_lhs = lhs; ent_rhs = rhs }
;;

let mk_func_defn name params body =
  let return_typ =
    match body with
    | None -> TUnk
    | Some e -> typ_of_exp e in
  { funcd_name = name;
    funcd_params = params;
    funcd_body = body;
    funcd_ret_typ = return_typ
  }
;;

let mk_reln_defn name params body =
  { relnd_name = name; relnd_params = params; relnd_body = body }
;;

let mk_data_defn typ params = { datad_typ = typ; datad_fields = params }

let mk_view_defn_case f =
  { vdc_id = -1; vdc_form = f; vdc_is_base_case = true }
;;

let mk_view_defn name params body =
  { viewd_name = name; viewd_params = params; viewd_body = body }
;;

(*** commands ***)

let mk_cmd_check_sat f = CheckSat f
let mk_cmd_prove_entailments ents = ProveEntails ents

let mk_program_empty () =
  { prog_file_name = "";
    prog_func_defns = [];
    prog_reln_defns = [];
    prog_data_defns = [];
    prog_view_defns = [];
    prog_proc_defns = [];
    prog_commands = []
  }
;;

(*******************************************************************
 ** equalities
 *******************************************************************)

type equality =
  | EqExp of (var * exp)
  | EqPure of (var * pure_form)

type equalities = equality list

let pr_equality (eq : equality) : string =
  match eq with
  | EqExp (v, e) -> pr_var v ^ "=" ^ pr_exp e
  | EqPure (v, p) -> pr_var v ^ "=(" ^ pr_pf p ^ ")"
;;

let pr_equalities (eqs : equalities) : string =
  eqs |> List.map ~f:pr_equality |> String.concat ~sep:" & "
;;

let fv_eq eq =
  match eq with
  | EqExp (v, e) -> insert_vs v (fv_e e)
  | EqPure (v, p) -> insert_vs v (fv_pf p)
;;

let fv_eqs eqs = eqs |> List.map ~f:fv_eq |> merge_vs

let collect_eq_exp_p (f : pure_form) : equalities =
  let rec collect p =
    match p with
    | Reln (Eq, [ e1; e2 ]) ->
      let res =
        match e1, e2 with
        | Var v1, Var v2 -> [ EqExp (v1, e2); EqExp (v2, e1) ]
        | Var v1, _ -> [ EqExp (v1, e2) ]
        | _, Var v2 -> [ EqExp (v2, e1) ]
        | Func (Sub, [ Var v; e ], _), Int 0 -> [ EqExp (v, e) ]
        | Func (Sub, [ e; Var v ], _), Int 0 -> [ EqExp (v, e) ]
        | _ -> [] in
      res
    | BEq (Var v, p) -> [ EqPure (v, p) ]
    | PConj gs -> gs |> List.map ~f:collect |> List.concat
    | PExists (vs, g) | PForall (vs, g) ->
      let gs = collect g in
      List.exclude ~f:(fun eq -> intersected_vs vs (fv_eq eq)) gs
    | _ -> [] in
  collect f
;;

let collect_eq_exp_f (f : formula) : equalities =
  let rec collect g =
    match g with
    | Pure p -> collect_eq_exp_p p
    | Star gs -> List.fold_left ~f:(fun acc f -> acc @ collect f) ~init:[] gs
    | Exists (vs, g0) ->
      let gs = collect g0 in
      List.exclude ~f:(fun eq -> intersected_vs vs (fv_eq eq)) gs
    | _ -> [] in
  collect f
;;

(*******************************************************************
 ** substitutions
 *******************************************************************)

type substitution = var -> exp

let reset_substitution_vs (sst : substitution) (vs : var list) v =
  if member_vs v vs then mk_exp_var v else sst v
;;

let init_subst () : substitution = fun v -> mk_exp_var v

let extend_subst (sst : substitution) (v : var) (e : exp) : substitution =
 fun u -> if equal_var v u then e else sst u
;;

let substitute_exp (sst : substitution) (e : exp) : exp =
  let rec subst e =
    match e with
    | Void _ | Null _ | Int _ | Float _ | String _ -> e
    | Var v -> sst v
    | Cast (e, t1, t2) -> Cast (subst e, t1, t2)
    | Func (f, es, t) -> Func (f, List.map ~f:subst es, t) in
  subst e
;;

let subst_addr_exp (sst : substitution) (a : addr_exp) =
  { addr_base = substitute_exp sst a.addr_base;
    addr_elem = substitute_exp sst a.addr_elem;
    addr_field = substitute_exp sst a.addr_field
  }
;;

let subst_reln_form (sst : substitution) (r : reln_form) =
  let rn, es = r in
  rn, List.map ~f:(substitute_exp sst) es
;;

let subst_rf = subst_reln_form

let substitute_pure_form (sst : substitution) (p : pure_form) =
  let rec subst sst p =
    match p with
    | Bool _ -> p
    | BExp e -> BExp (substitute_exp sst e)
    | BEq (e, p) -> BEq (substitute_exp sst e, subst sst p)
    | Reln r -> Reln (subst_rf sst r)
    | PNeg f -> PNeg (subst sst f)
    | PConj fs -> PConj (List.map ~f:(subst sst) fs)
    | PDisj fs -> PDisj (List.map ~f:(subst sst) fs)
    | PForall (vs, f) -> PForall (vs, subst (reset_substitution_vs sst vs) f)
    | PExists (vs, f) -> PExists (vs, subst (reset_substitution_vs sst vs) f)
  in
  subst sst p
;;

let subst_data_form (sst : substitution) (d : data_form) =
  { d with
    data_root = substitute_exp sst d.data_root;
    data_addr = Option.map ~f:(subst_addr_exp sst) d.data_addr;
    data_args = List.map ~f:(substitute_exp sst) d.data_args
  }
;;

let substitute_view_form (sst : substitution) (v : view_form) =
  { v with view_args = List.map ~f:(substitute_exp sst) v.view_args }
;;

let substitute_iter_form (sst : substitution) (i : iter_form) =
  { i with
    iter_base = substitute_exp sst i.iter_base;
    iter_element_index = substitute_exp sst i.iter_element_index;
    iter_begin = substitute_exp sst i.iter_begin;
    iter_end = substitute_exp sst i.iter_end
  }
;;

let substitute_array_form (sst : substitution) (a : array_form) =
  { a with
    array_root = substitute_exp sst a.array_root;
    array_size = substitute_exp sst a.array_size
  }
;;

let substitute_formula (sst : substitution) (f : formula) : formula =
  let rec subst sst f =
    match f with
    | Emp -> f
    | Pure p -> Pure (substitute_pure_form sst p)
    | Data d -> Data (subst_data_form sst d)
    | View v -> View (substitute_view_form sst v)
    | Iter a -> Iter (substitute_iter_form sst a)
    | Array a -> Array (substitute_array_form sst a)
    | Star fs -> Star (List.map ~f:(subst sst) fs)
    | Wand (f1, f2) -> Wand (subst sst f1, subst sst f2)
    | Septract (f1, f2) -> Septract (subst sst f1, subst sst f2)
    | Exists (vs, f) -> Exists (vs, subst (reset_substitution_vs sst vs) f)
  in
  subst sst f
;;

let get_subst_of_vars vars (eqs : equalities) : substitution =
  List.fold_left
    ~f:(fun acc eq ->
      match eq with
      | EqExp (v, e) when List.exists ~f:(equal_var v) vars ->
        if List.is_empty (intersect_vs (fv_e e) vars)
        then extend_subst acc v e
        else acc
      | _ -> acc)
    ~init:(init_subst ())
    eqs
;;

let get_substitute_formula_eqs (eqs : equalities) : substitution =
  let sst, _ =
    List.fold_left
      ~f:(fun (sst, used_vars) eq ->
        match eq with
        | EqExp (v, e) when not (List.exists ~f:(equal_var v) used_vars) ->
          let ssts = extend_subst sst v e in
          let used_vars = merge_vs [ used_vars; fv_e e ] in
          ssts, used_vars
        | _ -> sst, used_vars)
      ~init:(init_subst (), [])
      eqs in
  sst
;;

let mk_subst_vars_exps (vs : vars) (es : exps) : substitution =
  if List.length vs != List.length es
  then error "mk_subst_vars_exps: different length"
  else (
    let sst = init_subst () in
    List.fold2_exn ~f:(fun acc v e -> extend_subst acc v e) ~init:sst vs es)
;;

let mk_subst_vars_vars (vs1 : vars) (vs2 : vars) : substitution =
  mk_subst_vars_exps vs1 (List.map ~f:mk_exp_var vs2)
;;

(*******************************************************************
 ** renaming
 *******************************************************************)

type renaming = var -> var

let init_renaming () : renaming = fun v -> v

let extend_renaming (rnm : renaming) (v : var) (v' : var) : renaming =
 fun u -> if equal_var u v then v' else rnm u
;;

let cancel_renaming (rnm : renaming) (v : var) : renaming =
 fun u -> if equal_var u v then v else rnm u
;;

let mk_renaming_fresh (vs : var list) : renaming =
  List.fold_left
    ~f:(fun acc v -> extend_renaming acc v (fresh_old_var v))
    ~init:(init_renaming ())
    vs
;;

let mk_renaming_of_vars (vs1 : vars) (vs2 : vars) : renaming =
  if List.length vs1 != List.length vs2
  then error "mk_renaming_of_vars: different length"
  else
    List.fold2_exn
      ~f:(fun acc v1 v2 -> extend_renaming acc v1 v2)
      ~init:(init_renaming ())
      vs1
      vs2
;;

let rename_var (rnm : renaming) (v : var) : var = rnm v
let rename_v = rename_var
let rename_vs (rnm : renaming) (vs : var list) : var list = List.map ~f:rnm vs

let rename_exp (rnm : renaming) (e : exp) : exp =
  let rec rename e =
    match e with
    | Void _ | Null _ | Int _ | Float _ | String _ -> e
    | Var v -> Var (rnm v)
    | Cast (e, t1, t2) -> Cast (rename e, t1, t2)
    | Func (f, es, t) -> Func (f, List.map ~f:rename es, t) in
  rename e
;;

let rename_e = rename_exp

let rename_addr_exp (rnm : renaming) (a : addr_exp) =
  { addr_base = rename_e rnm a.addr_base;
    addr_elem = rename_e rnm a.addr_elem;
    addr_field = rename_e rnm a.addr_field
  }
;;

let rename_pure_form (rnm : renaming) (p : pure_form) =
  let rec rename p =
    match p with
    | Bool _ -> p
    | BExp e -> BExp (rename_e rnm e)
    | BEq (e, p) -> BEq (rename_e rnm e, rename p)
    | Reln (r, es) -> Reln (r, List.map ~f:(rename_e rnm) es)
    | PNeg f -> PNeg (rename f)
    | PConj fs -> PConj (List.map ~f:rename fs)
    | PDisj fs -> PDisj (List.map ~f:rename fs)
    | PForall (vs, f) -> PForall (rename_vs rnm vs, rename f)
    | PExists (vs, f) -> PExists (rename_vs rnm vs, rename f) in
  rename p
;;

let rename_pf = rename_pure_form

let rename_data_form (rnm : renaming) (d : data_form) =
  { d with
    data_root = rename_e rnm d.data_root;
    data_args = List.map ~f:(rename_e rnm) d.data_args;
    data_addr = Option.map ~f:(rename_addr_exp rnm) d.data_addr
  }
;;

let rename_df = rename_data_form

let rename_view_form (rnm : renaming) (v : view_form) =
  { v with view_args = List.map ~f:(rename_e rnm) v.view_args }
;;

let rename_vf = rename_view_form

let rename_iter_form (rnm : renaming) (i : iter_form) =
  let rnm = cancel_renaming rnm i.iter_running_index in
  { i with
    iter_base = rename_e rnm i.iter_base;
    iter_element_index = rename_e rnm i.iter_element_index;
    iter_begin = rename_e rnm i.iter_begin;
    iter_end = rename_e rnm i.iter_end
  }
;;

let rename_if = rename_iter_form

let rename_array_form (rnm : renaming) (a : array_form) =
  { a with
    array_root = rename_e rnm a.array_root;
    array_size = rename_e rnm a.array_size
  }
;;

let rename_af = rename_array_form

let rename_form (rnm : renaming) (f : formula) =
  let rec rename rnm f =
    match f with
    | Emp -> f
    | Pure p -> Pure (rename_pf rnm p)
    | Data d -> Data (rename_df rnm d)
    | View v -> View (rename_vf rnm v)
    | Iter i -> Iter (rename_if rnm i)
    | Array a -> Array (rename_af rnm a)
    | Star fs -> Star (List.map ~f:(rename rnm) fs)
    | Wand (f1, f2) -> Wand (rename rnm f1, rename rnm f2)
    | Septract (f1, f2) -> Septract (rename rnm f1, rename rnm f2)
    | Exists (vs, f) -> Exists (rename_vs rnm vs, rename rnm f) in
  rename rnm f
;;

let rename_f = rename_form

let rename_spec (rnm : renaming) (spec : proc_specs) : proc_specs =
  { psp_precond = rename_f rnm spec.psp_precond;
    psp_postcond = rename_f rnm spec.psp_postcond
  }
;;

let rename_specs (rnm : renaming) (specs : proc_specs list) : proc_specs list =
  List.map ~f:(rename_spec rnm) specs
;;

let rename_all_qvars_p (p : pure_form) : pure_form =
  let rec rename p =
    match p with
    | Bool _ | BExp _ | Reln _ -> p
    | PNeg p -> PNeg (rename p)
    | BEq (e, p) -> BEq (e, rename p)
    | PConj ps -> PConj (List.map ~f:rename ps)
    | PDisj ps -> PDisj (List.map ~f:rename ps)
    | PForall (vs, p) ->
      let nvs = List.map ~f:fresh_old_var vs in
      let rnm = mk_renaming_of_vars vs nvs in
      PForall (nvs, rename_pf rnm (rename p))
    | PExists (vs, p) ->
      let nvs = List.map ~f:fresh_old_var vs in
      let rnm = mk_renaming_of_vars vs nvs in
      PExists (nvs, rename_pf rnm (rename p)) in
  rename p
;;

let rename_all_qvars_f (f : formula) : formula =
  let rec rename f =
    match f with
    | Emp | Data _ | View _ | Iter _ | Array _ -> f
    | Pure p -> Pure (rename_all_qvars_p p)
    | Star fs -> Star (List.map ~f:rename fs)
    | Wand (f1, f2) -> Wand (rename f1, rename f2)
    | Septract (f1, f2) -> Septract (rename f1, rename f2)
    | Exists (vs, f) ->
      let nvs = List.map ~f:fresh_old_var vs in
      let rnm = mk_renaming_of_vars vs nvs in
      Exists (nvs, rename_f rnm (rename f)) in
  rename f
;;

(*******************************************************************
 ** translate LLVM IR to SL IR
 *******************************************************************)

let translate_ll_icmp (cmp : LL.Icmp.t) : relation_symbol =
  match cmp with
  | LL.Icmp.Eq -> Eq
  | LL.Icmp.Ne -> Ne
  | LL.Icmp.Ugt -> Gt
  | LL.Icmp.Uge -> Ge
  | LL.Icmp.Ult -> Lt
  | LL.Icmp.Ule -> Le
  | LL.Icmp.Sgt -> Gt
  | LL.Icmp.Sge -> Ge
  | LL.Icmp.Slt -> Lt
  | LL.Icmp.Sle -> Le
;;

let translate_ll_fcmp (cmp : LL.Fcmp.t) : relation_symbol =
  match cmp with
  | LL.Fcmp.False -> RName "LL.Fcmp.False"
  | LL.Fcmp.Oeq -> Eq
  | LL.Fcmp.Ogt -> Gt
  | LL.Fcmp.Oge -> Ge
  | LL.Fcmp.Olt -> Lt
  | LL.Fcmp.Ole -> Le
  | LL.Fcmp.One -> Ne
  | LL.Fcmp.Ord -> RName "LL.Fcmp.Ord"
  | LL.Fcmp.Uno -> RName "LL.Fcmp.Uno"
  | LL.Fcmp.Ueq -> Eq
  | LL.Fcmp.Ugt -> Gt
  | LL.Fcmp.Uge -> Ge
  | LL.Fcmp.Ult -> Lt
  | LL.Fcmp.Ule -> Le
  | LL.Fcmp.Une -> Ne
  | LL.Fcmp.True -> RName "LL.Fcmp.True"
;;

let rec translate_lltyp (typ : LL.lltype) : typ =
  match LL.classify_type typ with
  | LL.TypeKind.Void | LL.TypeKind.Label -> TVoid
  | LL.TypeKind.Half
  | LL.TypeKind.Float
  | LL.TypeKind.Double
  | LL.TypeKind.X86fp80
  | LL.TypeKind.Fp128
  | LL.TypeKind.Ppc_fp128 -> TFloat
  | LL.TypeKind.Integer -> TInt (LL.integer_bitwidth typ)
  | LL.TypeKind.Pointer -> TPointer (translate_lltyp (LL.element_type typ))
  | LL.TypeKind.Array -> TArray
  | LL.TypeKind.Struct ->
    let _ = debugc "TRANSLATE TYPE STRUCT" in
    TStruct (Option.value (LL.struct_name typ) ~default:"unknown")
  | LL.TypeKind.Function | LL.TypeKind.Vector | LL.TypeKind.X86_mmx -> TUnk
  | LL.TypeKind.Metadata -> error "translate_lltyp: do not expect Metadata"
  | _ -> TUnk
;;

let translate_llvalue (v : LL.llvalue) : exp =
  let typ = translate_lltyp (LL.type_of v) in
  if LI.is_llvalue_undef v
  then mk_null ()
  else (
    let vname = LL.value_name v in
    let res =
      match typ with
      | TInt _ ->
        (match LI.int_of_const v with
        | None -> mk_exp_var_typ vname typ
        | Some i -> mk_exp_int i)
      | TFloat ->
        (match LL.float_of_const v with
        | None -> mk_exp_var_typ (LL.value_name v) typ
        | Some i -> mk_exp_float i)
      | TString ->
        (match LL.string_of_const v with
        | None -> mk_exp_var_typ (LL.value_name v) typ
        | Some i -> mk_exp_string i)
      | TStruct _ | TPointer _ -> mk_exp_var_typ vname typ
      | TVoid -> mk_void ()
      | TUnk -> herror "translate_llvalue: unknown type of: " LI.pr_value v
      | _ -> herror "translate_llvalue: unhandled typ of" LI.pr_value v
    in
    res)
;;

let translate_instr (instr : LI.instr) : exp =
  translate_llvalue (LI.llvalue_of_instr instr)
;;

let translate_global (global : LI.global) : exp =
  translate_llvalue (LI.llvalue_of_global global)
;;

let translate_param (param : LI.param) : exp =
  translate_llvalue (LI.llvalue_of_param param)
;;

let get_operands (instr : LL.llvalue) index_begin index_end : exp list =
  let args = ref [] in
  for index = index_begin to index_end do
    args := !args @ [ translate_llvalue (LL.operand instr index) ]
  done;
  !args
;;

(*******************************************************************
 ** formula utilities
 *******************************************************************)

let extract_pure_form (f : formula) : pure_form =
  let rec extract f =
    match f with
    | Pure p -> p
    | Emp -> mk_pf_true ()
    | Data _ | View _ | Iter _ | Array _ | Star _ | Wand _ | Septract _ ->
      herror "extract_pure_form: not a pure formula" pr_f f
    | Exists (vs, g) -> mk_pexists vs (extract g) in
  extract f
;;

let mk_f_star_with ?(dfs = []) ?(vfs = []) ?(afs = []) ?(pfs = []) ?(fs = []) f
  =
  let pfs = List.exclude ~f:is_pf_true pfs in
  let nfs =
    List.map ~f:f_of_data dfs
    @ List.map ~f:f_of_view vfs
    @ List.map ~f:f_of_array afs
    @ List.map ~f:f_of_pure pfs
    @ fs in
  mk_f_star (f :: nfs)
;;

let mk_f_wand_with ?(dfs = []) ?(vfs = []) ?(afs = []) ?(pfs = []) ?(fs = []) f
  =
  let pfs = List.exclude ~f:is_pf_true pfs in
  let nfs =
    List.map ~f:f_of_data dfs
    @ List.map ~f:f_of_view vfs
    @ List.map ~f:f_of_array afs
    @ List.map ~f:f_of_pure pfs
    @ fs in
  let fleft = mk_f_star nfs in
  mk_f_wand fleft f
;;

let mk_eq_exps_pair (es1 : exps) (es2 : exps) : pure_form =
  try
    let pfs =
      List.fold2_exn
        ~f:(fun acc e1 e2 -> acc @ [ mk_eq e1 e2 ])
        ~init:[]
        es1
        es2 in
    mk_pconj pfs
  with _ -> error "mk_eq_exps_pair: lists of different length"
;;

(*******************************************************************
 ** function and relation queries
 *******************************************************************)

(* get function names *)

let collect_func_name_e (e : exp) : string SP.t =
  match e with
  | Func (FName fn, _, _) -> SP.singleton fn
  | _ -> SP.empty
;;

let collect_func_name_p (p : pure_form) : string SP.t =
  let rec get_func f =
    match f with
    | Reln (_, es) -> SP.union_list (List.map ~f:collect_func_name_e es)
    | PNeg f | PForall (_, f) | PExists (_, f) -> get_func f
    | PConj fs | PDisj fs -> SP.union_list (List.map ~f:get_func fs)
    | _ -> SP.empty in
  get_func p
;;

let collect_func_name_ps (ps : pure_forms) : string SP.t =
  SP.union_list (List.map ~f:collect_func_name_p ps)
;;

let collect_func_name_df (d : data_form) : string SP.t =
  SP.union_list (List.map ~f:collect_func_name_e d.data_args)
;;

let collect_func_name_vf (v : view_form) : string SP.t =
  SP.union_list (List.map ~f:collect_func_name_e v.view_args)
;;

let collect_func_name_if (i : iter_form) : string SP.t =
  let s1 = collect_func_name_e i.iter_base in
  let s2 = collect_func_name_e i.iter_element_index in
  let s3 = collect_func_name_e i.iter_begin in
  let s4 = collect_func_name_e i.iter_end in
  SP.union_list [ s1; s2; s3; s4 ]
;;

let collect_func_name_af (a : array_form) : string SP.t =
  let s1 = collect_func_name_e a.array_root in
  let s2 = collect_func_name_e a.array_size in
  SP.union_list [ s1; s2 ]
;;

let collect_func_name_f (f : formula) : string SP.t =
  let rec get_name = function
    | Emp -> SP.empty
    | Pure p -> collect_func_name_p p
    | Data d -> collect_func_name_df d
    | View v -> collect_func_name_vf v
    | Iter a -> collect_func_name_if a
    | Array a -> collect_func_name_af a
    | Star fs -> SP.union_list (List.map ~f:get_name fs)
    | Wand (f1, f2) | Septract (f1, f2) ->
      SP.union_list [ get_name f1; get_name f2 ]
    | Exists (_, g) -> get_name g in
  get_name f
;;

let collect_func_name_fs (fs : formulas) : string SP.t =
  SP.union_list (List.map ~f:collect_func_name_f fs)
;;

(* get relation names *)

let collect_reln_name_p (p : pure_form) : string SP.t =
  let rec get_rel f =
    match f with
    | Reln (RName rn, es) -> SP.singleton rn
    | PNeg f | PForall (_, f) | PExists (_, f) -> get_rel f
    | PConj fs | PDisj fs -> SP.union_list (List.map ~f:get_rel fs)
    | _ -> SP.empty in
  get_rel p
;;

let collect_reln_name_ps (ps : pure_forms) : string SP.t =
  SP.union_list (List.map ~f:collect_reln_name_p ps)
;;

let collect_reln_name_f (f : formula) : string SP.t =
  let rec get_rel f =
    match f with
    | Emp | Data _ | View _ | Iter _ | Array _ -> SP.empty
    | Pure p -> collect_reln_name_p p
    | Star fs -> SP.union_list (List.map ~f:get_rel fs)
    | Wand (f1, f2) | Septract (f1, f2) ->
      SP.union_list [ get_rel f1; get_rel f2 ]
    | Exists (_, f) -> get_rel f in
  get_rel f
;;

let collect_reln_name_fs (fs : formulas) : string SP.t =
  SP.union_list (List.map ~f:collect_reln_name_f fs)
;;

(* get view names *)

let collect_view_name_f (f : formula) : string SP.t =
  let rec get_view g =
    match g with
    | Pure _ | Emp | Data _ | Iter _ | Array _ -> SP.empty
    | View v -> SP.singleton v.view_name
    | Star fs -> SP.union_list (List.map ~f:get_view fs)
    | Wand (f1, f2) | Septract (f1, f2) ->
      SP.union_list [ get_view f1; get_view f2 ]
    | Exists (_, f) -> get_view f in
  get_view f
;;

let collect_view_name_fs (fs : formulas) : string SP.t =
  SP.union_list (List.map ~f:collect_view_name_f fs)
;;

(*******************************************************************
 ** Definition queries
 *******************************************************************)

let find_func_defn fdefns (fname : string) : func_defn option =
  List.find ~f:(fun d -> String.equal d.funcd_name fname) fdefns
;;

let find_reln_defn rdefns (pname : string) : reln_defn option =
  List.find ~f:(fun d -> String.equal d.relnd_name pname) rdefns
;;

let find_data_defn ddefns (dname : string) : data_defn option =
  List.find ~f:(fun d -> String.equal (pr_type d.datad_typ) dname) ddefns
;;

let find_view_defn vdefns (vname : string) : view_defn option =
  List.find ~f:(fun d -> String.equal d.viewd_name vname) vdefns
;;

let has_view_defn prog vname =
  match find_view_defn prog.prog_view_defns vname with
  | None -> false
  | Some _ -> true
;;

(*******************************************************************
 ** unfolding relation and view
 *******************************************************************)

let unfold_view_form vdefns (vf : view_form) : view_defn_case list =
  match find_view_defn vdefns vf.view_name with
  | None -> []
  | Some vd ->
    let sst = mk_subst_vars_exps vd.viewd_params vf.view_args in
    List.map
      ~f:(fun vdc ->
        let f = vdc.vdc_form |> rename_all_qvars_f |> substitute_formula sst in
        { vdc with vdc_form = f })
      vd.viewd_body
;;

(*******************************************************************
 ** replacing relation and view
 *******************************************************************)

let replace_reln_form_p (head : reln_form) (body : pure_form) p : pure_form =
  let rec replace p =
    match p, head with
    | Bool _, _ | BExp _, _ -> p
    | Reln (RName rnp, esp), (RName rnh, esh) when String.equal rnp rnh ->
      let args, pfs =
        List.fold_left
          ~f:(fun (aargs, apfs) e ->
            match e with
            | Var v -> aargs @ [ v ], apfs
            | _ ->
              let v = fresh_new_var (typ_of_exp e) in
              aargs @ [ v ], apfs @ [ mk_eq (mk_exp_var v) e ])
          ~init:([], [])
          esh in
      let body =
        let vs = diff_vs (fv_pf body) (fv_es esh) in
        let rnm = mk_renaming_fresh vs in
        let body = rename_pf rnm body in
        mk_pconj (body :: pfs) in
      let sst = mk_subst_vars_exps args esp in
      substitute_pure_form sst body
    | Reln _, _ -> p
    | PNeg p, _ -> mk_pneg (replace p)
    | BEq (e, p), _ -> BEq (e, replace p)
    | PConj ps, _ -> mk_pconj (List.map ~f:replace ps)
    | PDisj ps, _ -> mk_pdisj (List.map ~f:replace ps)
    | PForall (vs, p), _ -> mk_pforall vs (replace p)
    | PExists (vs, p), _ -> mk_pexists vs (replace p) in
  replace p
;;

let replace_reln_form_f (head : reln_form) (body : pure_form) f : formula =
  let rec replace f =
    match f with
    | Pure p -> mk_f_pure (replace_reln_form_p head body p)
    | Emp | Data _ | View _ | Iter _ | Array _ -> f
    | Star fs -> mk_f_star (List.map ~f:replace fs)
    | Wand (f1, f2) | Septract (f1, f2) -> mk_f_wand (replace f1) (replace f2)
    | Exists (vs, f) -> mk_f_exists vs (replace f) in
  replace f
;;

let replace_view_form_f (head : view_form) (body : formula) f : formula =
  let rec replace f =
    match f with
    | Emp | Data _ | Pure _ | Iter _ | Array _ -> f
    | View vf ->
      if String.equal vf.view_name head.view_name
      then (
        let args, pfs =
          List.fold_left
            ~f:(fun (aargs, apfs) e ->
              match e with
              | Var v -> aargs @ [ v ], apfs
              | _ ->
                let v = fresh_new_var (typ_of_exp e) in
                aargs @ [ v ], apfs @ [ mk_eq (mk_exp_var v) e ])
            ~init:([], [])
            head.view_args in
        let body =
          let vs = diff_vs (fv_f body) (fv_es head.view_args) in
          let rnm = mk_renaming_fresh vs in
          let body = rename_f rnm body in
          mk_f_star [ body; mk_f_pure (mk_pconj pfs) ] in
        let sst = mk_subst_vars_exps args vf.view_args in
        substitute_formula sst body)
      else f
    | Star fs -> mk_f_star (List.map ~f:replace fs)
    | Wand (f1, f2) -> mk_f_wand (replace f1) (replace f2)
    | Septract (f1, f2) -> mk_f_septract (replace f1) (replace f2)
    | Exists (vs, f) -> mk_f_exists vs (replace f) in
  replace f
;;

(*******************************************************************
 ** utilities
 *******************************************************************)

let update_array_form (a : array_form) dfs =
  { a with array_update = a.array_update @ dfs }
;;

let subtract_array_form (a : array_form) dfs =
  { a with array_subtract = a.array_subtract @ dfs }
;;

(* TODO: need to find both root and index *)
let find_addr_exp_of_exp (f : formula) (e : exp) : addr_exp list =
  let rec find_root g =
    match g with
    | Emp | Pure _ | View _ -> []
    | Data d ->
      if equal_exp d.data_root e then [ mk_addr_base d.data_root ] else []
    | Array a ->
      if equal_exp a.array_root e
      then [ mk_addr_base a.array_root ]
      else (
        let ds =
          List.find ~f:(fun d -> equal_exp d.data_root e) a.array_update in
        match ds with
        | None -> []
        | Some d ->
          (match d.data_addr with
          | None -> []
          | Some a -> [ a ]))
    | Star gs -> List.fold_left ~f:(fun acc g -> acc @ find_root g) ~init:[] gs
    | Iter _ | Wand _ | Septract _ -> []
    | Exists (_, g) -> find_root g in
  find_root f
;;
