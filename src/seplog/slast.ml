(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
include Slvar

type bin_op =
  | Add
  | Sub
  | Mul
  | Div

type exp =
  | Int of (int * position)
  | Float of (float * position)
  | String of (string * position)
  | Var of (var * position)
  | BinExp of (bin_op * exp * exp * position)
  | Func of (string * exp list * typ * position)

type addr_exp =
  { (* similar to getelementptr of LLVM*)
    addr_base : exp
  ; addr_elem : exp
  ; addr_field : exp
  }

type bin_rel =
  | Eq
  | Ne
  | Le
  | Lt
  | Ge
  | Gt
[@@deriving equal]

type formula =
  | Bool of (bool * position)
  | BVar of (var * position)
  | BEq of (exp * formula)
  | Emp of position
  | BinRel of (bin_rel * exp * exp * position)
  | Data of (exp * typ * exp list * addr_exp option * position)
  | Pred of (string * exp list * position)
  | Array of (exp * exp * typ * position)
  | Neg of formula
  | Conj of (formula * formula)
  | Disj of (formula * formula)
  | Star of (formula * formula)
  | Wand of (formula * formula)
  | Septract of (formula * formula)
  | Update of (formula * formula)
  | Forall of (var list * formula)
  | Exists of (var list * formula)

type entailment =
  { ent_id : int
  ; ent_rhs : formula
  ; ent_lhs : formula
  }

type func_defn =
  { funcd_name : string
  ; funcd_params : var list
  ; funcd_body : exp option
  ; funcd_ret_typ : typ
  ; funcd_loc : position
  }

type data_defn =
  { datad_typ : typ
  ; datad_fields : (typ * string) list
  ; datad_loc : position
  }

type pred_typ =
  | PtView
  | PtReln
[@@deriving equal]

type pred_defn =
  { predd_name : string
  ; predd_params : var list
  ; predd_body : formula list
  ; predd_typ : pred_typ
  ; predd_loc : position
  }

type proc_specs =
  { psp_precond : formula
  ; psp_postcond : formula
  }

type proc_defn =
  { procd_name : string
  ; procd_params : var list
  ; procd_return_typ : typ
  ; procd_specs : proc_specs list
  ; procd_loc : position
  }

type command =
  | CheckSat of (formula * position)
  | ProveEntails of (entailment list * position)
  | InferFrame of (entailment * position)

type program =
  { prog_file_name : string
  ; prog_func_defns : func_defn list
  ; prog_data_defns : data_defn list
  ; prog_pred_defns : pred_defn list
  ; prog_proc_defns : proc_defn list
  ; prog_commands : command list
  }

(*******************************************************************
 ** constructors
 *******************************************************************)

let rec typ_of_exp e =
  match e with
  | Int (_, _) -> TInt 32
  | Float (_, _) -> TFloat
  | String (_, _) -> TString
  | Var (v, _) -> typ_of_var v
  | BinExp (Add, e1, e2, _)
  | BinExp (Sub, e1, e2, _)
  | BinExp (Mul, e1, e2, _)
  | BinExp (Div, e1, e2, _) ->
    let t1, t2 = typ_of_exp e1, typ_of_exp e2 in
    let res =
      match t1, t2 with
      | TInt i1, TInt i2 -> TInt (max i1 i2)
      | _ -> error "typ_of_exp: expect int type" in
    res
  | Func (_, _, t, _) -> t
;;

let mk_entailment ?(id = 0) lhs rhs =
  { ent_id = id; ent_lhs = lhs; ent_rhs = rhs }
;;

let mk_func_defn name params body loc =
  let return_typ =
    match body with
    | None -> TUnk
    | Some e -> typ_of_exp e in
  { funcd_name = name
  ; funcd_params = params
  ; funcd_body = body
  ; funcd_ret_typ = return_typ
  ; funcd_loc = loc
  }
;;

let mk_data_defn typ params loc =
  { datad_typ = typ; datad_fields = params; datad_loc = loc }
;;

let mk_pred_defn name params ptype body loc =
  { predd_name = name
  ; predd_params = params
  ; predd_typ = ptype
  ; predd_body = body
  ; predd_loc = loc
  }
;;

let mk_proc_specs pre post = { psp_precond = pre; psp_postcond = post }

let mk_proc_defn name params rtyp specs loc =
  { procd_name = name
  ; procd_params = params
  ; procd_return_typ = rtyp
  ; procd_specs = specs
  ; procd_loc = loc
  }
;;

let mk_program_empty () =
  { prog_file_name = ""
  ; prog_func_defns = []
  ; prog_data_defns = []
  ; prog_pred_defns = []
  ; prog_proc_defns = []
  ; prog_commands = []
  }
;;

(*******************************************************************
 ** printing
 *******************************************************************)

let rec sprint_exp (e : exp) : string =
  match e with
  | Int (i, _) -> sprint_int i
  | Float (f, _) -> sprint_float f
  | String (s, _) -> "\"" ^ s ^ "\""
  | Var (v, _) -> sprint_var v
  | BinExp (Add, e1, e2, _) -> sprint_exp e1 ^ "+" ^ sprint_exp e2
  | BinExp (Sub, e1, e2, _) -> sprint_exp e1 ^ "-" ^ sprint_exp e2
  | BinExp (Mul, e1, e2, _) -> sprint_exp e1 ^ "\\*" ^ sprint_exp e2
  | BinExp (Div, e1, e2, _) -> sprint_exp e1 ^ "/" ^ sprint_exp e2
  | Func (fn, es, _, _) -> fn ^ "(" ^ sprint_exps es ^ ")"

and sprint_exps (es : exp list) : string = sprint_list ~sep:"," ~f:sprint_exp es

let pr_addr_exp (a : addr_exp) : string =
  "("
  ^ sprint_exp a.addr_base
  ^ ","
  ^ sprint_exp a.addr_elem
  ^ ","
  ^ sprint_exp a.addr_field
  ^ ")"
;;

let rec sprint_formula (f : formula) : string =
  match f with
  | Bool (b, _) -> sprint_bool b
  | BVar (v, _) -> sprint_var v
  | BEq (e, f) -> sprint_exp e ^ "=" ^ "(" ^ sprint_formula f ^ ")"
  | Emp _ -> "emp"
  | BinRel (Eq, e1, e2, _) -> sprint_exp e1 ^ "=" ^ sprint_exp e2
  | BinRel (Ne, e1, e2, _) -> sprint_exp e1 ^ "!=" ^ sprint_exp e2
  | BinRel (Lt, e1, e2, _) -> sprint_exp e1 ^ "<" ^ sprint_exp e2
  | BinRel (Le, e1, e2, _) -> sprint_exp e1 ^ "<=" ^ sprint_exp e2
  | BinRel (Gt, e1, e2, _) -> sprint_exp e1 ^ ">" ^ sprint_exp e2
  | BinRel (Ge, e1, e2, _) -> sprint_exp e1 ^ ">=" ^ sprint_exp e2
  | Data (e, typ, es, addr, _) ->
    let addr =
      match addr with
      | None -> ""
      | Some a -> "@" ^ pr_addr_exp a in
    sprint_exp e ^ "->" ^ sprint_type typ ^ "{" ^ sprint_exps es ^ "}" ^ addr
  | Pred (pn, es, _) -> pn ^ "(" ^ sprint_exps es ^ ")"
  | Array (e1, e2, t, _) ->
    "array(" ^ sprint_exp e1 ^ "," ^ sprint_exp e2 ^ "," ^ sprint_type t ^ ")"
  | Neg g -> "!" ^ "(" ^ sprint_formula g ^ ")"
  | Conj (f1, f2) -> sprint_formula f1 ^ " & " ^ sprint_formula f2
  | Disj (f1, f2) -> sprint_formula f1 ^ " | " ^ sprint_formula f2
  | Star (f1, f2) -> sprint_formula f1 ^ " * " ^ sprint_formula f2
  | Wand (f1, f2) -> sprint_formula f1 ^ " --* " ^ sprint_formula f2
  | Septract (f1, f2) -> sprint_formula f1 ^ " *- " ^ sprint_formula f2
  | Update (f1, f2) -> sprint_formula f1 ^ " *+ " ^ sprint_formula f2
  | Forall (vs, f) -> "(exists " ^ sprint_vars vs ^ ". " ^ sprint_formula f ^ ")"
  | Exists (vs, f) -> "(forall " ^ sprint_vars vs ^ ". " ^ sprint_formula f ^ ")"
;;

let sprint_ent (ent : entailment) : string =
  let res = sprint_formula ent.ent_lhs ^ " |- " ^ sprint_formula ent.ent_rhs in
  if ent.ent_id < 1 then res else "#" ^ sprint_int ent.ent_id ^ ". " ^ res
;;

let sprint_ents (ents : entailment list) : string =
  hsprint_list_itemized ~bullet:"  # " ~f:sprint_ent ents
;;

(*** print declarations ***)

let sprint_data_defn (d : data_defn) : string =
  let fields =
    d.datad_fields
    |> List.map ~f:(fun (t, n) -> "  " ^ sprint_type t ^ " " ^ n ^ ";")
    |> String.concat ~sep:"\n" in
  "data " ^ sprint_type d.datad_typ ^ "{\n" ^ fields ^ "\n};"
;;

let sprint_predicate_defn (p : pred_defn) : string =
  let pred_type =
    match p.predd_typ with
    | PtView -> "view"
    | PtReln -> "rel" in
  let header =
    pred_type ^ " " ^ p.predd_name ^ "(" ^ sprint_vars p.predd_params ^ ") := "
  in
  let body =
    match p.predd_body with
    | [] -> "?"
    | fs -> sprint_list ~sep:"\n    \\/ " ~f:sprint_formula fs in
  header ^ body ^ ";"
;;

let sprint_func_defn (f : func_defn) : string =
  let header = "func " ^ f.funcd_name ^ "(" ^ sprint_vars f.funcd_params ^ ") := " in
  let body =
    match f.funcd_body with
    | None -> "?"
    | Some e -> sprint_exp e in
  header ^ body ^ ";"
;;

let sprint_proc_defn (p : proc_defn) : string =
  let params =
    p.procd_params
    |> List.map ~f:(fun v -> sprint_type (typ_of_var v) ^ " " ^ sprint_var v)
    |> String.concat ~sep:"," in
  let specs =
    p.procd_specs
    |> List.map ~f:(fun s ->
           "  requires "
           ^ sprint_formula s.psp_precond
           ^ "\n"
           ^ "  ensures "
           ^ sprint_formula s.psp_postcond
           ^ ";")
    |> String.concat ~sep:"\n" in
  sprint_type p.procd_return_typ ^ " " ^ p.procd_name ^ "(" ^ params ^ ")\n" ^ specs
;;

let sprint_command (c : command) =
  match c with
  | CheckSat (f, _) -> "CheckSat: " ^ sprint_formula f ^ ";"
  | ProveEntails (ents, _) -> "ProveEntails: \n" ^ sprint_ents ents ^ ";"
  | InferFrame (ent, _) -> "FindFrame: " ^ sprint_ent ent ^ ";"
;;

let sprint_program (p : program) : string =
  let funcs = p.prog_func_defns |> List.map ~f:sprint_func_defn in
  let datas = p.prog_data_defns |> List.map ~f:sprint_data_defn in
  let preds = p.prog_pred_defns |> List.map ~f:sprint_predicate_defn in
  let procs = p.prog_proc_defns |> List.map ~f:sprint_proc_defn in
  let cmds = p.prog_commands |> List.map ~f:sprint_command in
  String.concat ~sep:"\n\n" (funcs @ datas @ preds @ procs @ cmds)
;;

(*******************************************************************
 ** Function and relation queries
 *******************************************************************)

let find_func_defn (prog : program) (fname : string) : func_defn option =
  List.find ~f:(fun d -> String.equal d.funcd_name fname) prog.prog_func_defns
;;

let find_data_defn (prog : program) (dname : string) : data_defn option =
  List.find
    ~f:(fun d -> String.equal (sprint_type d.datad_typ) dname)
    prog.prog_data_defns
;;

let find_pred_defn (prog : program) (pname : string) : pred_defn option =
  List.find ~f:(fun d -> String.equal d.predd_name pname) prog.prog_pred_defns
;;
