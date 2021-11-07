(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Sprinter
open Printer
open Debugger
module SA = Slast
module SI = Slir

type pred_defn_kind =
  | PReln of SI.reln_defn
  | PView of SI.view_defn

let transform_bin_op (op : SA.bin_op) : SI.func_symbol =
  match op with
  | SA.Add -> SI.Add
  | SA.Sub -> SI.Sub
  | SA.Mul -> SI.Mul
  | SA.Div -> SI.Div
;;

let rec transform_exp (e : SA.exp) : SI.exp =
  match e with
  | SA.Int (i, _) -> SI.Int i
  | SA.Float (f, _) -> SI.Float f
  | SA.String (s, _) -> SI.String s
  | SA.Var (v, _) -> SI.Var v
  | SA.BinExp (op, e1, e2, _) ->
    SI.Func (transform_bin_op op, transform_exps [ e1; e2 ], SA.typ_of_exp e)
  | SA.Func (fname, es, typ, _) -> SI.Func (FName fname, transform_exps es, typ)

and transform_exps (es : SA.exp list) : SI.exp list =
  List.map ~f:transform_exp es
;;

let transform_addr_exp (a : SA.addr_exp) : SI.addr_exp =
  { SI.addr_base = transform_exp a.SA.addr_base
  ; SI.addr_elem = transform_exp a.SA.addr_elem
  ; SI.addr_field = transform_exp a.SA.addr_field
  }
;;

let transform_bin_rel (rel : SA.bin_rel) : SI.relation_symbol =
  match rel with
  | SA.Eq -> SI.Eq
  | SA.Ne -> SI.Ne
  | SA.Le -> SI.Le
  | SA.Lt -> SI.Lt
  | SA.Ge -> SI.Ge
  | SA.Gt -> SI.Gt
;;

let rec transform_formula (prog : SA.program) (f : SA.formula) : SI.formula =
  match f with
  | SA.Bool (b, _) -> SI.mk_f_pure (SI.Bool b)
  | SA.BVar (v, _) -> SI.mk_f_pure (SI.mk_bexp (SI.Var v))
  | SA.BEq (e, f0) ->
    let p =
      match transform_formula prog f0 with
      | Pure p -> p
      | _ -> herror "transform_formula: BEq: not a pure" SA.pr_formula f in
    SI.mk_f_pure (SI.mk_beq (transform_exp e) p)
  | SA.Emp l -> SI.mk_emp ()
  | SA.BinRel (rel, e1, e2, _) ->
    let rel, es = transform_bin_rel rel, transform_exps [ e1; e2 ] in
    SI.mk_f_pure (SI.mk_preln rel es)
  | SA.Data (root, typ, args, addr, _) ->
    let root, args = transform_exp root, transform_exps args in
    let addr = Option.map ~f:transform_addr_exp addr in
    SI.mk_f_data typ root args ~addr
  | SA.Pred (pname, args, _) ->
    let args = transform_exps args in
    (match SA.find_pred_defn prog pname with
    | None -> SI.mk_f_view pname args
    | Some pd ->
      if SA.equal_pred_typ pd.SA.predd_typ SA.PtReln
      then SI.mk_f_pure (SI.mk_preln (SI.RName pname) args)
      else SI.mk_f_view pname args)
  | SA.Array (root, size, etyp, _) ->
    let root, size = transform_exp root, transform_exp size in
    SI.mk_f_array (SI.mk_array root size etyp)
  | SA.Neg f0 ->
    let p =
      match transform_formula prog f0 with
      | SI.Pure p -> p
      | _ -> herror "transform_formula: Neg: not a pure" SA.pr_formula f in
    SI.mk_f_pure (SI.mk_pneg p)
  | SA.Conj (f1, f2) ->
    let p1, p2 =
      match transform_formula prog f1, transform_formula prog f2 with
      | SI.Pure p1, SI.Pure p2 -> p1, p2
      | _ -> herror "transform_formula: Conj: not a pure" SA.pr_formula f in
    SI.mk_f_pure (SI.mk_pconj [ p1; p2 ])
  | SA.Disj (f1, f2) ->
    let p1, p2 =
      match transform_formula prog f1, transform_formula prog f2 with
      | SI.Pure p1, SI.Pure p2 -> p1, p2
      | _ -> herror "transform_formula: Disj: not a pure" SA.pr_formula f in
    SI.mk_f_pure (SI.mk_pdisj [ p1; p2 ])
  | SA.Star (f1, f2) ->
    let f1, f2 = transform_formula prog f1, transform_formula prog f2 in
    SI.mk_f_star [ f1; f2 ]
  | SA.Wand (f1, f2) ->
    let f1, f2 = transform_formula prog f1, transform_formula prog f2 in
    SI.mk_f_wand f1 f2
  | SA.Septract (f1, f2) ->
    let f1, f2 = transform_formula prog f1, transform_formula prog f2 in
    SI.mk_f_septract f1 f2
  | SA.Update (f1, f2) ->
    let f1, f2 = transform_formula prog f1, transform_formula prog f2 in
    (match f1, f2 with
    | SI.Array a, SI.Data d -> SI.Array (SI.update_array_form a [ d ])
    | _ -> error "transform_formula: fail to translate SA.Update formula")
  | SA.Forall (vs, f0) ->
    (match transform_formula prog f0 with
    | SI.Pure p -> Pure (SI.mk_pforall vs p)
    | g -> herror "transform_formula: Forall: expect pure: " SA.pr_formula f)
  | SA.Exists (vs, f0) ->
    (match transform_formula prog f0 with
    | SI.Pure p -> Pure (SI.mk_pforall vs p)
    | g -> SI.mk_f_exists vs g)
;;

let transform_formulas prog (fs : SA.formula list) : SI.formula list =
  List.map ~f:(transform_formula prog) fs
;;

let transform_entailment prog (ent : SA.entailment) : SI.entailment =
  let lhs = transform_formula prog ent.SA.ent_lhs in
  let rhs = transform_formula prog ent.SA.ent_rhs in
  SI.mk_entailment ~id:ent.ent_id lhs rhs
;;

let transform_func_defn prog (fd : SA.func_defn) : SI.func_defn =
  { SI.funcd_name = fd.SA.funcd_name
  ; SI.funcd_params = fd.SA.funcd_params
  ; SI.funcd_body = Option.map ~f:transform_exp fd.SA.funcd_body
  ; SI.funcd_ret_typ = fd.SA.funcd_ret_typ
  }
;;

let transform_data_defn prog (dd : SA.data_defn) : SI.data_defn =
  { SI.datad_typ = dd.SA.datad_typ; SI.datad_fields = dd.SA.datad_fields }
;;

let transform_pred_defn prog (pd : SA.pred_defn) =
  let name, params = pd.SA.predd_name, pd.SA.predd_params in
  let body = transform_formulas prog pd.SA.predd_body in
  match pd.SA.predd_typ, body with
  | SA.PtReln, [] -> PReln (SI.mk_reln_defn name params None)
  | SA.PtReln, [ f ] ->
    let body =
      match f with
      | SI.Pure p -> Some p
      | _ -> herror "transform_pred_reln: not a pure body" SI.pr_formula f in
    PReln (SI.mk_reln_defn name params body)
  | SA.PtReln, _ -> error ("transform_pred_reln: expect 1 body form: " ^ name)
  | SA.PtView, fs ->
    let vdcs = List.map ~f:SI.mk_view_defn_case fs in
    PView (SI.mk_view_defn name params vdcs)
;;

let transform_proc_specs prog (sp : SA.proc_specs) : SI.proc_specs =
  let pre = transform_formula prog sp.SA.psp_precond in
  let post = transform_formula prog sp.SA.psp_postcond in
  { SI.psp_precond = pre; SI.psp_postcond = post }
;;

let transform_proc_defn prog (pd : SA.proc_defn) =
  let specs = List.map ~f:(transform_proc_specs prog) pd.SA.procd_specs in
  { SI.procd_name = pd.SA.procd_name
  ; SI.procd_params = pd.SA.procd_params
  ; SI.procd_return_typ = pd.SA.procd_return_typ
  ; SI.procd_specs = specs
  }
;;

let transform_command prog (cmd : SA.command) : SI.command =
  match cmd with
  | SA.CheckSat (f, _) -> SI.CheckSat (transform_formula prog f)
  | SA.ProveEntails (ents, _) ->
    SI.ProveEntails (List.map ~f:(transform_entailment prog) ents)
  | SA.InferFrame (ent, _) -> SI.InferFrame (transform_entailment prog ent)
;;

let transform_program (prog : SA.program) : SI.program =
  let funcs = List.map ~f:(transform_func_defn prog) prog.SA.prog_func_defns in
  let datas = List.map ~f:(transform_data_defn prog) prog.SA.prog_data_defns in
  let relns, views =
    List.fold_left
      ~f:(fun (rds, vds) pd ->
        match transform_pred_defn prog pd with
        | PView vd -> rds, vds @ [ vd ]
        | PReln rd -> rds @ [ rd ], vds)
      ~init:([], [])
      prog.SA.prog_pred_defns in
  let procs = List.map ~f:(transform_proc_defn prog) prog.SA.prog_proc_defns in
  let cmds = List.map ~f:(transform_command prog) prog.SA.prog_commands in
  { SI.prog_file_name = prog.SA.prog_file_name
  ; SI.prog_func_defns = funcs
  ; SI.prog_reln_defns = relns
  ; SI.prog_data_defns = datas
  ; SI.prog_view_defns = views
  ; SI.prog_proc_defns = procs
  ; SI.prog_commands = cmds
  }
;;
