(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Z3ir
module LL = Llvm
module PS = Outils.Process
module SI = Slir
module LI = Llir
module SP = Set.Poly

(*******************************************************************
 ** Z3 process management
 *******************************************************************)

let z3_exe = ref "z3"
let z3_version = ref "unknown"
let z3_eof = "Z3EOF"
let z3_cmd = [ !z3_exe; "-smt2"; "-in"; "-t:5000" ]

(* let proc = ref (PS.mk_proc_dummy [ !z3_cmd; "-smt2"; "-in"; "-t:5000" ]) *)

let z3proc = ref None

let config_z3_solver () : unit =
  let open Option.Let_syntax in
  let _ =
    match PS.run_command_get_output [ "which"; !z3_exe ] with
    | Ok res -> z3_exe := res
    | Error msg -> () in
  match PS.run_command_get_output [ !z3_exe; "--version" ] with
  | Ok output ->
    ignore
      (let%bind line = String.find_line_contain ~pattern:"version" output in
       let%bind version = String.slice_from ~pattern:"version" line in
       let version =
         String.substr_replace_all ~pattern:"version " ~with_:"v" version in
       return (z3_version := version))
  | Error msg -> error "Z3 version not found!"
;;

let start_solver () =
  let send_config proc config =
    output_string proc.PS.proc_out_channel config;
    flush proc.PS.proc_out_channel in
  match !z3proc with
  | None ->
    let config = [] in
    (match PS.open_process z3_cmd with
    | Ok proc ->
      let _ = z3proc := Some proc in
      List.iter ~f:(fun cfg -> send_config proc cfg) config
    | Error log ->
      error
        (("Failed to start Z3: %s" ^ String.concat ~sep:" " z3_cmd ^ "\n")
        ^ "Error log: " ^ log))
  | _ -> ()
;;

let stop_solver () =
  match !z3proc with
  | Some proc ->
    let _ = PS.close_process proc in
    z3proc := None
  | None -> ()
;;

let restart_solver () =
  debug "Restarting Z3 ...";
  let _ = stop_solver () in
  let _ = Unix.nanosleep 0.005 in
  start_solver ()
;;

let send_input_to_solver (input : string) : unit =
  match !z3proc with
  | None -> warning "send_input_to_solver: Z3 is not running"
  | Some proc ->
    let input = "(reset)\n" ^ input ^ "(echo \"" ^ z3_eof ^ "\")\n" in
    (* let _ = debugpc "Z3bin INPUT: \n" pr_str input in *)
    output_string proc.PS.proc_out_channel input;
    flush proc.PS.proc_out_channel
;;

let read_output_from_solver () : string =
  match !z3proc with
  | None ->
    let _ = warning "read_output_from_solver: Z3 is not running" in
    ""
  | Some proc ->
    let rec read acc =
      try
        let line = String.strip (input_line proc.PS.proc_in_channel) in
        (* let _ = debugpc "Z3bin OUTPUT: " pr_str line in *)
        let nacc = acc @ [ line ] in
        if String.equal line z3_eof then nacc else read nacc
      with End_of_file -> read acc in
    let output = [] |> read |> String.concat ~sep:"\n" in
    (* let _ = debugpc "Z3bin OUTPUT FINAL: " pr_str output in *)
    output
;;

let read_all_output proc : string =
  let rec read acc =
    try
      let line = String.strip (input_line proc.PS.proc_in_channel) in
      let () = debug ("line: " ^ line) in
      let nacc = acc @ [ line ] in
      read nacc
    with End_of_file -> acc in
  let res = String.concat ~sep:"\n" (read []) in
  (* let _ = debugpc "Z3bin output: " pr_str res in *)
  res
;;

(*******************************************************************
 ** Z3 interface for SL IR
 *******************************************************************)

module Z3SL = struct
  let transform_bool b = if b then "true" else "false"
  let transform_int i = pr_int i
  let transform_int64 i = pr_int64 i
  let transform_float f = pr_float f
  let transform_string s = "\"" ^ s ^ "\""

  let transform_typ (t : SI.typ) : string =
    match t with
    | TBool -> "Bool"
    | TInt _ -> "Int"
    | _ -> "Int"
  ;;

  (* FIXME: temporarily put as Int type *)

  let transform_var (v : SI.var) : string = SI.pr_var v

  let transform_typed_vars (vs : SI.var list) =
    vs
    |> List.map ~f:(fun x ->
           "(" ^ transform_var x ^ " " ^ transform_typ (SI.typ_of_var x) ^ ")")
    |> String.concat ~sep:" "
  ;;

  let rec transform_exp (e : SI.exp) : string =
    match e with
    | Void _ -> "0"
    | Null _ -> "0"
    | Int i -> transform_int i
    | Int64 i -> transform_int64 i
    | Float f -> transform_float f
    | String s -> transform_string s
    | Var v -> transform_var v
    | Cast (e, _, _) ->
      transform_exp e (* TODO: need to consider casted type *)
    | Func (Add, [ e1; e2 ], _) ->
      "(+ " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Func (Sub, [ e1; e2 ], _) ->
      "(- " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Func (Mul, [ e1; e2 ], _) ->
      "(* " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Func (Div, [ e1; e2 ], _) ->
      "(/ " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Func (FName n, es, _) -> "(" ^ n ^ " " ^ transform_exps es ^ ")"
    | Func _ -> error "z3: transform_exp: unexpected function: (need details)"

  and transform_exps (es : SI.exp list) =
    es |> List.map ~f:transform_exp |> String.concat ~sep:" "

  and transform_pure_form (f : SI.pure_form) : string =
    match f with
    | Bool b -> transform_bool b
    | BExp e -> transform_exp e
    | BEq (e, f) -> "(= " ^ transform_exp e ^ " " ^ transform_pure_form f ^ ")"
    | Reln (Eq, [ e1; e2 ]) ->
      let se1, se2 = transform_exp e1, transform_exp e2 in
      "(= " ^ se1 ^ " " ^ se2 ^ ")"
    | Reln (Ne, [ e1; e2 ]) ->
      "(not (= " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ "))"
    | Reln (Lt, [ e1; e2 ]) ->
      "(< " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Reln (Gt, [ e1; e2 ]) ->
      "(> " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Reln (Le, [ e1; e2 ]) ->
      "(<= " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Reln (Ge, [ e1; e2 ]) ->
      "(>= " ^ transform_exp e1 ^ " " ^ transform_exp e2 ^ ")"
    | Reln (RName n, es) -> "(" ^ n ^ " " ^ transform_exps es ^ ")"
    | Reln _ -> error "z3: transform_atom: unexpected relation: (need details)"
    | PNeg f -> sprintf "(not " ^ transform_pure_form f ^ ")"
    | PConj fs -> "(and " ^ transform_forms fs ^ ")"
    | PDisj fs -> "(or " ^ transform_forms fs ^ ")"
    | PForall (vs, g) ->
      let svs = transform_typed_vars vs in
      let sg = transform_pure_form g in
      "(forall (" ^ svs ^ ") " ^ sg ^ ")"
    | PExists (vs, g) ->
      let svs = transform_typed_vars vs in
      let sg = transform_pure_form g in
      "(exists (" ^ svs ^ ") " ^ sg ^ ")"

  and transform_forms fs =
    fs |> List.map ~f:transform_pure_form |> String.concat ~sep:" "
  ;;

  let mk_var_decl v =
    let typ = transform_typ (snd v) in
    "(declare-const " ^ transform_var v ^ " " ^ typ ^ ")"
  ;;

  let mk_func_defn (fd : SI.func_defn) : string =
    match fd.funcd_body with
    | None ->
      let param_typs =
        fd.funcd_params
        |> List.map ~f:(fun a -> a |> SI.typ_of_var |> transform_typ)
        |> String.concat ~sep:" " in
      let return_typ = fd.funcd_ret_typ |> transform_typ in
      "(declare-fun "
      ^ (fd.funcd_name ^ (" (" ^ param_typs ^ ") ") ^ return_typ)
      ^ ")"
    | Some e ->
      let param_typs =
        fd.funcd_params
        |> List.map ~f:(fun x ->
               let sx, tx = transform_var x, transform_typ (SI.typ_of_var x) in
               "(" ^ sx ^ " " ^ tx ^ ")")
        |> String.concat ~sep:" " in
      let return_typ = transform_typ (SI.typ_of_exp e) in
      let body = transform_exp e in
      "(define-fun "
      ^ (fd.funcd_name ^ (" (" ^ param_typs ^ ") ") ^ return_typ)
      ^ body ^ ")"
  ;;

  let mk_reln_defn (pd : SI.reln_defn) : string =
    let return_typ = "Bool" in
    match pd.relnd_body with
    | None ->
      let param_typs =
        pd.relnd_params
        |> List.map ~f:(fun x -> transform_typ (SI.typ_of_var x))
        |> String.concat ~sep:" " in
      Printf.sprintf "(declare-fun %s (%s) %s)" pd.relnd_name param_typs
        return_typ
    | Some f ->
      let params =
        pd.relnd_params
        |> List.map ~f:(fun x ->
               "(" ^ transform_var x ^ " "
               ^ transform_typ (SI.typ_of_var x)
               ^ ")")
        |> String.concat ~sep:" " in
      let body = transform_pure_form f in
      Printf.sprintf "(define-fun %s (%s) %s %s)" pd.relnd_name params
        return_typ body
  ;;

  let mk_assert f = "(assert " ^ transform_pure_form f ^ ")"

  let mk_input ?(prog = None) ?(mvars = []) (fs : SI.pure_form list) : string =
    let concat_newlines str =
      if String.equal str "" then str else str ^ "\n\n" in
    let var_decls =
      fs |> SI.fv_pfs |> ( @ ) mvars |> SI.dedup_vs |> List.map ~f:mk_var_decl
      |> String.concat ~sep:"\n" in
    let func_decls =
      match prog with
      | None -> ""
      | Some prog ->
        let fnames = SI.collect_func_name_ps fs in
        let fdefns =
          SP.fold
            ~f:(fun acc fn ->
              match SI.find_func_defn prog.SI.prog_func_defns fn with
              | None -> acc
              | Some fd -> SP.add acc (mk_func_defn fd))
            ~init:SP.empty fnames in
        String.concat ~sep:"\n" (SP.to_list fdefns) in
    let reln_decls =
      match prog with
      | None -> ""
      | Some prog ->
        let rnames = SI.collect_reln_name_ps fs in
        let rdefns =
          SP.fold
            ~f:(fun acc rn ->
              match SI.find_reln_defn prog.prog_reln_defns rn with
              | None -> acc
              | Some rd -> SP.add acc (mk_reln_defn rd))
            ~init:SP.empty rnames in
        String.concat ~sep:"\n" (SP.to_list rdefns) in
    let assertions = fs |> List.map ~f:mk_assert |> String.concat ~sep:"\n\n" in
    concat_newlines var_decls ^ concat_newlines func_decls
    ^ concat_newlines reln_decls ^ assertions
  ;;

  (* return the result and the model if existing *)
  let get_result ?(mvars = []) z3output : SI.smt_result =
    match z3output with
    | Sat model ->
      let model =
        List.fold_left
          ~f:(fun acc (s, i) ->
            try
              let var =
                List.find_exn ~f:(fun v -> String.equal (SI.pr_var v) s) mvars
              in
              acc @ [ var, SI.mk_exp_int i ]
            with _ -> acc)
          ~init:[] (norm_model model) in
      (* sort decreasingly *)
      let model =
        List.sort ~compare:(fun (v1, _) (v2, _) -> SI.compare_var v2 v1) model
      in
      Some true, model
    | Unsat -> Some true, []
    | Unk -> None, []
    | Error _ -> None, []
  ;;

  let check_sat ?(prog = None) ?(logic = "") ?(mvars = []) fs : SI.smt_result =
    let set_logic =
      if String.equal logic "" then "" else "(set-logic " ^ logic ^ ")\n" in
    let set_option_model, get_model =
      match mvars with
      | [] -> "(set-option :produce-models false)\n", ""
      | _ -> "(set-option :produce-models true)\n", "(get-model)\n" in
    let z3_input =
      Printf.sprintf "%s%s\n%s\n\n%s\n%s" set_logic set_option_model
        (mk_input ~prog ~mvars fs) "(check-sat)" get_model in
    let _ = start_solver () in
    let _ = send_input_to_solver z3_input in
    let z3_output = read_output_from_solver () in
    let lexbuf = Lexing.from_string z3_output in
    let output = Z3parser.output Z3lexer.tokenizer lexbuf in
    match output with
    | Error msg ->
      let msg =
        "Z3SL: error while checking sat:\n"
        ^ (String.halign_line "  - formula: " SI.pr_pfs fs ^ "\n")
        ^ (String.align_line "  - z3 input:\n" z3_input ^ "\n")
        ^ (String.align_line "  - z3 output:\n" z3_output ^ "\n")
        ^ "  - error: " ^ msg ^ "\n" in
      let _ = debug msg in
      None, []
    | _ ->
      let res, model = get_result ~mvars output in
      res, model
  ;;

  let check_imply ?(prog = None) ?(logic = "") f1 f2 : bool option =
    let open Option.Let_syntax in
    let nf2 = SI.mk_pneg f2 in
    let res, _ = check_sat ~prog ~logic [ f1; nf2 ] in
    let%bind b = res in
    Option.return (not b)
  ;;
end

(*******************************************************************
 ** Z3 interface for LLVM IR
 *******************************************************************)

module Z3LL = struct
  let transform_list (f : 'a -> string) (lst : 'a list) : string =
    String.concat ~sep:" " (List.map ~f lst)
  ;;

  let transform_lltype (t : LI.datatype) : string =
    match LL.classify_type t with
    | LL.TypeKind.Integer -> "Int"
    | LL.TypeKind.Pointer -> "Int" (* handle pointer as Int type *)
    | _ -> errorp "Z3LL.transform_lltype: need to handle type: " LI.pr_type t
  ;;

  let transform_llvalue (v : LI.value) : string =
    (* NOTE: should include function name to avoid name duplication? *)
    if LL.is_null v then "0" else LI.pr_value v
  ;;

  let transform_predicate (p : LI.predicate) : string =
    let rec transform p =
      match p with
      | LI.PBool b -> if b then "true" else "false"
      | PIcmp (cmp, lhs, rhs) ->
        let slhs, srhs = transform_llvalue lhs, transform_llvalue rhs in
        (match cmp with
        | LL.Icmp.Eq -> "(= " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Ne -> "(not (= " ^ slhs ^ " " ^ srhs ^ "))"
        | LL.Icmp.Ugt -> "(> " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Uge -> "(>= " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Ult -> "(< " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Ule -> "(<= " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Sgt -> "(> " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Sge -> "(>= " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Slt -> "(< " ^ slhs ^ " " ^ srhs ^ ")"
        | LL.Icmp.Sle -> "(<= " ^ slhs ^ " " ^ srhs ^ ")")
      | PFcmp _ -> error "z3: handle Fcmp later"
      | PNeg p1 -> "(not " ^ transform p1 ^ ")"
      | PConj ps -> "(and " ^ transform_list transform ps ^ ")"
      | PDisj ps -> "(or " ^ transform_list transform ps ^ ")" in
    transform p
  ;;

  let mk_var_decl (v : LI.value) : string =
    let sv = transform_llvalue v in
    let st = transform_lltype (LL.type_of v) in
    "(declare-const " ^ sv ^ " " ^ st ^ ")"
  ;;

  let mk_assertion (p : LI.predicate) : string =
    "(assert " ^ transform_predicate p ^ ")\n"
  ;;

  let mk_z3_input (p : LI.predicate) : string =
    let vars =
      p |> LI.collect_llvalue_of_predicate
      |> List.exclude ~f:(fun v ->
             LI.is_llvalue_null_constant v || LI.is_llvalue_integer_constant v)
    in
    let var_decls =
      vars |> List.map ~f:mk_var_decl |> String.concat ~sep:"\n" in
    let assertion = mk_assertion p in
    var_decls ^ "\n" ^ assertion
  ;;

  let get_result z3output : bool option =
    match z3output with
    | Sat model -> Some true
    | Unsat -> Some false
    | _ -> None
  ;;

  let check_sat (p : LI.predicate) : bool option =
    let z3_input =
      "(set-option :produce-models false)\n"
      ^ (mk_z3_input p ^ "\n")
      ^ "(check-sat)\n" in
    let _ = start_solver () in
    let _ = send_input_to_solver z3_input in
    let z3_output = read_output_from_solver () in
    let lexbuf = Lexing.from_string z3_output in
    let output = Z3parser.output Z3lexer.tokenizer lexbuf in
    match output with
    | Error msg ->
      let msg =
        "Z3LL: error while checking sat:\n"
        ^ (String.halign_line "  - predicate: " LI.pr_predicate p ^ "\n")
        ^ (String.align_line "  - z3 input:\n" z3_input ^ "\n")
        ^ (String.align_line "  - z3 output:\n" z3_output ^ "\n")
        ^ String.align_line "  - error: " msg in
      let _ = debug msg in
      None
    | _ -> get_result output
  ;;
end
