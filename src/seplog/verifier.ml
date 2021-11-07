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
open Slir
open Bug
module PV = Prover
module LL = Llvm
module LI = Llir
module LD = Lldebug
module OC = Llvm.Opcode
module LTD = Llvm_target.DataLayout
module NO = Normalize
module AG = Arguments
module SMT = Smt.SmtSL

(*******************************************************************
 ** basic data structures
 *******************************************************************)

let prefix_precond = "pre"
let prefix_postcond = "post"
let data_layout = ref ""

type program_state =
  { pgs_formula : formula
  ; pgs_next_block : LI.block option
  ; pgs_alloced_pointers : exp list
  ; pgs_freed_pointers : exp list
  ; pgs_reachable : bool
  ; pgs_is_buggy : bool
  ; pgs_last_instr : LI.instr option
  ; pgs_entails : entailments
  }

type program_states = program_state list

type verifier_state =
  { vrs_core_prog : program
  ; vrs_llvm_prog : LI.program
  ; vrs_recent_instr : LI.llvalue option
  ; mutable vrs_interact : bool
  }

let mk_verifier_state (prog : LI.program) =
  { vrs_core_prog = mk_program_empty ()
  ; vrs_llvm_prog = prog
  ; vrs_recent_instr = None
  ; vrs_interact = !mode_interactive_prover
  }
;;

(*******************************************************************
 ** global variables
 *******************************************************************)

let lib_core = ref (mk_program_empty ())

(*******************************************************************
 ** printers and constructors
 *******************************************************************)

let enable_vrs_interact vstate = vstate.vrs_interact <- true

let update_recent_instruction vstate instr =
  { vstate with vrs_recent_instr = Some instr }
;;

let pr_program_states pstates =
  pstates
  |> List.map ~f:(fun ps -> "// { " ^ pr_f ps.pgs_formula ^ " }")
  |> String.concat ~sep:"\n"
;;

let mk_program_state (f : formula) (ents : entailments) : program_state =
  { pgs_formula = f
  ; pgs_next_block = None
  ; pgs_alloced_pointers = []
  ; pgs_freed_pointers = []
  ; pgs_reachable = true
  ; pgs_is_buggy = false
  ; pgs_last_instr = None
  ; pgs_entails = ents
  }
;;

(*******************************************************************
 ** utility functions
 *******************************************************************)

let propagate_program_state_entails pstate ents =
  { pstate with pgs_entails = pstate.pgs_entails @ ents }
;;

let propagate_program_state_freed_pointers pstate ptrs =
  { pstate with pgs_freed_pointers = pstate.pgs_freed_pointers @ ptrs }
;;

let mk_star_program_states
    ?(dfs = [])
    ?(vfs = [])
    ?(pfs = [])
    ?(afs = [])
    pstates
  =
  List.map
    ~f:(fun ps ->
      let f = mk_f_star_with ps.pgs_formula ~dfs ~vfs ~pfs ~afs in
      { ps with pgs_formula = f })
    pstates
;;

let find_host_alloca_pointers pstate ptrs =
  let rec collect_host f =
    match f with
    | Emp | Pure _ | View _ | Iter _ | Array _ -> []
    | Data df ->
      if List.exists ~f:(fun v -> mem_exp v ptrs) df.data_args
      then [ df.data_root ]
      else []
    | Star fs -> List.fold ~f:(fun acc f -> acc @ collect_host f) ~init:[] fs
    | Wand _ | Septract _ -> [] (* FIXME: need to handle this *)
    | Exists (_, f) -> collect_host f in
  collect_host pstate.pgs_formula
;;

let find_source_pointers pstate ptrs =
  ptrs @ find_host_alloca_pointers pstate ptrs
;;

let get_original_freed_pointers pstate ptrs : exp list =
  match pstate.pgs_last_instr with
  | None ->
    let _ = warning "get_freed_pointer: unknown last instruction" in
    []
  | Some instr ->
    (match LI.instr_opcode instr with
    | LO.BitCast ->
      let src = translate_llvalue (LI.operand instr 0) in
      let dst = translate_instr instr in
      if mem_exp dst ptrs then [ src ] else []
    | _ ->
      let _ = error "get_freed_pointer: last instruction is not BitCast" in
      [])
;;

(*******************************************************************
 ** checking bugs
 *******************************************************************)

let pr_buggy_exps prog exps : string =
  let cur_exps = pr_list ~sep:", " pr_exp exps in
  match !llvm_orig_source_name with
  | false -> cur_exps
  | true ->
    let source_exps =
      exps
      |> List.fold
           ~f:(fun acc e ->
             match e with
             | Var v ->
               (match
                  Hashtbl.find prog.LI.prog_llvalue_original_name (pr_var v)
                with
               | None -> acc
               | Some v -> acc @ [ v ])
             | _ -> acc)
           ~init:[]
      |> String.concat ~sep:", " in
    if !location_source_code_only
    then source_exps
    else cur_exps ^ " (llvm) ~~ " ^ source_exps ^ " (source)"
;;

let report_bug prog bug exps (instr : LI.instr) : bool =
  let location =
    match !llvm_orig_source_name, LD.position_of_instr instr with
    | false, _ | _, None -> ""
    | true, Some p -> "   at " ^ pr_file_position_and_excerpt p in
  let msg =
    "#########################################################\n"
    ^ " DETECTING A BUG: "
    ^ bug
    ^ "\n"
    ^ "   on the pointers: "
    ^ pr_buggy_exps prog exps
    ^ "\n"
    ^ location
    ^ "#########################################################\n" in
  let _ = print_endline ("\n" ^ msg) in
  true
;;

let report_bug_of_exps prog msg exps =
  let sexps = pr_list ~sep:", " pr_exp exps in
  report_bug prog (msg ^ sexps)
;;

let check_memory_leak vstate (instr : LI.instr) pstates : bool =
  let rec collect_heap_pointer f =
    match f with
    | Emp | Pure _ -> []
    | Data df -> [ df.data_root ]
    | View vf -> [] (* FIXME: leak of view? *)
    | Iter _ -> [] (* FIXME: need to handle this *)
    | Array af -> [ af.array_root ]
    | Star fs ->
      List.fold ~f:(fun acc f -> acc @ collect_heap_pointer f) ~init:[] fs
    | Septract _ -> [] (* FIXME: need to handle this *)
    | Wand _ -> [] (* FIXME: need to handle this *)
    | Exists (_, f) -> collect_heap_pointer f in
  let prog = vstate.vrs_llvm_prog in
  let buggy_exps =
    List.fold
      ~f:(fun acc ps ->
        let heap_ptrs = collect_heap_pointer ps.pgs_formula in
        let leaked_ptrs =
          List.fold
            ~f:(fun acc2 v -> acc2 @ find_source_pointers ps [ v ])
            ~init:[]
            heap_ptrs in
        acc @ leaked_ptrs)
      ~init:[]
      pstates in
  if buggy_exps != []
  then report_bug prog "MEMORY LEAK" buggy_exps instr
  else false
;;

let check_use_after_free vstate instr pstate ptr : bool =
  let prog = vstate.vrs_llvm_prog in
  let parent_ptrs =
    [ ptr ]
    |> find_source_pointers pstate
    |> List.filter ~f:(fun e -> mem_exp e pstate.pgs_freed_pointers) in
  if List.is_empty parent_ptrs
  then false
  else report_bug prog "USE AFTER FREE" (parent_ptrs @ [ ptr ]) instr
;;

let check_double_free vstate instr pstate func args : bool =
  let prog = vstate.vrs_llvm_prog in
  let freed_ptrs = get_original_freed_pointers pstate args in
  let parent_ptrs =
    freed_ptrs
    |> find_source_pointers pstate
    |> List.filter ~f:(fun e -> mem_exp e pstate.pgs_freed_pointers) in
  if not (LI.is_func_free func)
  then false
  else if LI.is_func_free func && List.is_empty parent_ptrs
  then false
  else report_bug prog "DOUBLE FREE" (parent_ptrs @ args) instr
;;

let check_null_pointer_deref vstate instr pstate ptr : bool =
  let prog = vstate.vrs_llvm_prog in
  let report_null_pointer_deref ptrs instr =
    report_bug prog "NULL POINTER DEREFERENCE" ptrs instr in
  match is_expr_null ptr with
  | true -> report_null_pointer_deref [ ptr ] instr
  | false ->
    let parent_ptrs =
      [ ptr ]
      |> find_source_pointers pstate
      |> List.exclude ~f:(fun e -> mem_exp e pstate.pgs_freed_pointers) in
    (match parent_ptrs with
    | [] -> false
    | _ -> report_null_pointer_deref (parent_ptrs @ [ ptr ]) instr)
;;

let check_buffer_overflow vstate instr pstate ptr size index : bool =
  let prog = vstate.vrs_llvm_prog in
  let pf = NO.encode_formula_to_pure pstate.pgs_formula in
  let overflow_cond = mk_ge index size in
  if is_true (SMT.check_imply pf overflow_cond)
  then (
    let buggy_exps = find_source_pointers pstate [ ptr ] @ [ ptr ] in
    report_bug prog "BUFFER OVERFLOW" buggy_exps instr)
  else false
;;

let check_integer_overflow instr pstate ptr : bool =
  (* TODO: implement *)
  (* Templates:
     let pf = NO.encode_formula_to_pure pstate.pgs_formula in
     let overflow_cond = ... in
     let nf = mk_pconj [pf; overflow_cond] in
     if Smt.check_sat nf = True then
       report bug
     else false *)
  false
;;

(*******************************************************************
 ** utility functions for symbolic execution
 *******************************************************************)

let find_lib_proc pname : proc_defn option =
  let procs =
    List.filter
      ~f:(fun p -> String.equal p.procd_name pname)
      !lib_core.prog_proc_defns in
  match procs with
  | [] -> None
  | [ p ] -> Some p
  | _ -> error ("lib_core has duplicated procedures named: " ^ pname)
;;

let encode_block_pre_state blk : view_form =
  let func = LI.func_of_block blk in
  let vname = LI.block_name blk ^ LI.func_name func in
  let llargs =
    if LI.is_entry_block_of_function blk
    then func |> LI.func_params |> List.map ~f:LI.llvalue_of_param
    else
      (func |> LI.func_params |> List.map ~f:LI.llvalue_of_param)
      @ (func |> LI.local_vars_of_func |> List.map ~f:LI.llvalue_of_instr) in
  let args = List.map ~f:translate_llvalue llargs in
  mk_view vname args
;;

let encode_precond_llproc (func : LI.func) : view_form =
  let args = func |> LI.func_params |> List.map ~f:translate_param in
  let vname = prefix_precond ^ LI.func_name func in
  mk_view vname args
;;

let encode_postcond_llproc (func : LI.func) : view_form =
  let args = func |> LI.formal_params_of_func |> List.map ~f:translate_param in
  let return_typ =
    func
    |> LI.llvalue_of_func
    |> LL.type_of
    |> LL.return_type
    |> translate_lltyp in
  let returns =
    match return_typ with
    | TVoid -> []
    | _ -> [ mk_exp_var_typ "ret" return_typ ] in
  let vname = prefix_postcond ^ LI.func_name func in
  mk_view vname (args @ returns)
;;

let get_prepost_llproc (func : LI.func) (args : exp list) (res : exp) =
  let pname = LI.func_name func in
  match find_lib_proc pname with
  | None ->
    let precond = mk_f_view (prefix_precond ^ pname) args in
    let postcond =
      let all_args =
        match typ_of_exp res with
        | TVoid -> args
        | _ -> args @ [ res ] in
      mk_f_view (prefix_postcond ^ pname) all_args in
    precond, postcond
  | Some pd ->
    let proc_res = mk_var __result (typ_of_exp res) in
    let proc_specs =
      let proc_params = proc_res :: pd.procd_params in
      let free_vs = diff_vs (fv_specs pd.procd_specs) proc_params in
      let rnm = mk_renaming_fresh free_vs in
      rename_specs rnm pd.procd_specs in
    let sst =
      let sst = mk_subst_vars_exps pd.procd_params args in
      extend_subst sst proc_res res in
    (match proc_specs with
    | [] -> error ("get_precond_llproc: no specs of func: " ^ pname)
    | [ sp ] ->
      ( substitute_formula sst sp.psp_precond
      , substitute_formula sst sp.psp_postcond )
    | _ -> error ("get_precond_llproc: many specs of func: " ^ pname))
;;

(*******************************************************************
 ** analyze detailed instruction
 *******************************************************************)

(*** instruction return ***)

let analyze_instr_return vstate pstates (instr : LI.instr) =
  let _ = check_memory_leak vstate instr pstates in
  let src = translate_llvalue (LI.src_of_instr_return instr) in
  let typ = typ_of_exp src in
  let dst = mk_exp_var_typ "ret" typ in
  let pf = mk_eq dst src in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction branching ***)

let analyze_instr_br vstate pstates (instr : LI.instr) =
  let branch = LI.branch_of_instr_br instr in
  match branch with
  | None -> error "analyze_instr_br: branch not found"
  | Some (`Unconditional blk) ->
    List.map ~f:(fun ps -> { ps with pgs_next_block = Some blk }) pstates
  | Some (`Conditional (cond_llval, blk1, blk2)) ->
    let cond = LI.operand instr 0 |> translate_llvalue |> mk_bexp in
    List.fold
      ~f:(fun acc s ->
        let f1 = mk_f_star_with s.pgs_formula ~pfs:[ cond ] in
        let f2 = mk_f_star_with s.pgs_formula ~pfs:[ mk_pneg cond ] in
        let s1 = { s with pgs_next_block = Some blk1 } in
        let s2 = { s with pgs_next_block = Some blk2 } in
        acc @ [ s1; s2 ])
      ~init:[]
      pstates
;;

(*** instruction unreachable ***)

let analyze_instr_unreachable vstate pstates (instr : LI.instr) =
  let ents =
    List.map ~f:(fun ps -> mk_entailment ps.pgs_formula (mk_f_false ())) pstates
  in
  List.map
    ~f:(fun ps ->
      let ent = mk_entailment ps.pgs_formula (mk_f_false ()) in
      { ps with
        pgs_formula = mk_f_false ()
      ; pgs_next_block = None
      ; pgs_reachable = false
      ; pgs_entails = ps.pgs_entails @ [ ent ]
      })
    pstates
;;

(*** instruction arithmetic ***)

let analyze_instr_arith vstate pstates (instr : LI.instr) =
  let exp0 = translate_llvalue (LI.operand instr 0) in
  let exp1 = translate_llvalue (LI.operand instr 1) in
  let op = LI.instr_opcode instr in
  let src =
    match op with
    | OC.Add | OC.FAdd -> mk_add exp0 exp1
    | OC.Sub | OC.FSub -> mk_sub exp0 exp1
    | OC.Mul | OC.FMul -> mk_mul exp0 exp1
    | OC.UDiv | OC.SDiv | OC.FDiv -> mk_div exp0 exp1
    | _ -> herror "analyze_instr_arith: unhandle opcode" LI.pr_opcode op in
  let dst = translate_instr instr in
  let pf = mk_eq dst src in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction alloca ***)

let analyze_instr_alloca vstate pstates (instr : LI.instr) =
  (* TODO: need to check the typ here is array or struct? *)
  let alloc_lltyp = LL.element_type (LI.type_of_instr instr) in
  match LL.classify_type alloc_lltyp with
  | LL.TypeKind.Array ->
    let elem_typ = translate_lltyp alloc_lltyp in
    let size = mk_exp_int (LL.array_length alloc_lltyp) in
    let root = translate_instr instr in
    let array = mk_array root size elem_typ in
    mk_star_program_states pstates ~afs:[ array ]
  | LL.TypeKind.Struct ->
    let struct_typ = translate_lltyp alloc_lltyp in
    let root = translate_instr instr in
    let elem_typs =
      alloc_lltyp
      |> LL.struct_element_types
      |> Array.to_list
      |> List.map ~f:translate_lltyp in
    let init_elems =
      List.map ~f:(fun typ -> mk_exp_var (fresh_new_var typ)) elem_typs in
    let _ = hdebugc "Struct Typ: " pr_type struct_typ in
    let _ = hdebugc "Root Typ: " pr_type (typ_of_exp root) in
    let data = mk_data struct_typ root init_elems in
    mk_star_program_states pstates ~dfs:[ data ]
  | _ ->
    let _ = warning "analyze_instr_alloca: unknown alloca type" in
    pstates
;;

(*** instruction load ***)

let check_bug_inst_load prog instr pstate src : bool =
  let b1 = check_null_pointer_deref prog instr pstate src in
  let b2 = check_use_after_free prog instr pstate src in
  let res = b1 && b2 in
  let _ = if res then warning "There is a bug in instr Load" in
  res
;;

let process_one_state_inst_load vstate instr src dst pstate =
  let elem_typ = get_pointer_elem_typ (typ_of_exp src) in
  let data_value = fresh_exp_var ~name:"t" elem_typ in
  let curr_data = mk_f_data elem_typ src [ data_value ] in
  let evs = fv_es [ data_value ] in
  let ent = mk_entailment pstate.pgs_formula (mk_f_exists evs curr_data) in
  let _ = hdebugc "LOAD: Prove Pre-Condition By Frame:\n\n" pr_ent ent in
  let res, frames = PV.infer_entailment_frame vstate.vrs_core_prog ent in
  let _ = hdebugc "==> Result: " pr_bresult res in
  let _ = hdebugc "==> Frame: " pr_fs frames in
  match res, frames with
  | Some true, [ frame ] ->
    let npf = mk_eq dst data_value in
    let nf =
      mk_f_exists evs (mk_f_star_with frame ~fs:[ curr_data ] ~pfs:[ npf ])
    in
    let nf = NO.simplify_f nf in
    let eqs =
      nf
      |> collect_eq_exp_f
      |> List.filter ~f:(function
             | EqExp (v, e) -> equal_exp e dst
             | EqPure _ -> false) in
    let sst = get_substitute_formula_eqs eqs in
    let nf, _ = nf |> substitute_formula sst |> NO.simplify_tauto_contra_f in
    { pstate with pgs_formula = nf }
  | _ ->
    let _ = check_bug_inst_load vstate instr pstate src in
    pstate
;;

let analyze_instr_load vstate pstates (instr : LI.instr) =
  let src = translate_llvalue (LI.operand instr 0) in
  let dst = translate_instr instr in
  List.map ~f:(process_one_state_inst_load vstate instr src dst) pstates
;;

(*** instruction store ***)

let check_bug_instr_store prog instr pstate dst : bool =
  let b1 = check_null_pointer_deref prog instr pstate dst in
  let b2 = check_use_after_free prog instr pstate dst in
  let res = b1 && b2 in
  let _ = if res then warning "There is a bug in instr Store" in
  res
;;

let process_one_state_instr_store vstate instr dst new_val pstate =
  let elem_typ = get_pointer_elem_typ (typ_of_exp dst) in
  let data_value = fresh_exp_var ~name:"t" elem_typ in
  let curr_data = mk_f_data elem_typ dst [ data_value ] in
  let evs = fv_e data_value in
  let ent = mk_entailment pstate.pgs_formula (mk_f_exists evs curr_data) in
  let _ = hdebugc "STORE: Entailment:\n" pr_ent ent in
  let res, frames = PV.infer_entailment_frame vstate.vrs_core_prog ent in
  let _ = hdebugc "==> Result: " pr_bresult res in
  let _ = hdebugc "==> Frame: " pr_fs frames in
  match res, frames with
  | Some true, [ frame ] ->
    let new_data = mk_data elem_typ dst [ new_val ] in
    let nf = mk_f_exists evs (mk_f_star_with frame ~dfs:[ new_data ]) in
    let nf = NO.simplify_f nf in
    { pstate with pgs_formula = nf }
  | _ ->
    let _ = check_bug_instr_store vstate instr pstate dst in
    pstate
;;

let analyze_instr_store vstate pstates (instr : LI.instr) =
  let new_val = translate_llvalue (LI.operand instr 0) in
  let dst = translate_llvalue (LI.operand instr 1) in
  List.map ~f:(process_one_state_instr_store vstate instr dst new_val) pstates
;;

(*** instruction getelementptr ***)

let process_getelemptr_array vstate instr pstate src index field dst =
  let src_etyp = get_pointer_elem_typ (typ_of_exp src) in
  let src_size = mk_exp_var (fresh_new_var (TInt 32)) in
  let src_evs = fv_es [ src_size ] in
  let src_array, dst_addr =
    let addrs = find_addr_exp_of_exp pstate.pgs_formula src in
    match addrs with
    | [] -> herror "process_getelemptr: array root not found:" pr_exp src
    | [ a ] ->
      let src_size = src_size |> mk_add a.addr_elem |> NO.simplify_arith_e in
      let src_array = mk_array a.addr_base src_size src_etyp in
      let dst_index = index |> mk_add a.addr_elem |> NO.simplify_arith_e in
      let dst_addr = mk_addr a.addr_base dst_index (mk_exp_int 0) in
      src_array, dst_addr
    | _ -> herror "process_getelemptr: too many array cells of:" pr_exp src
  in
  (* let _ = hdebugc "Src array: " pr_array_form src_array in *)
  let ent =
    let rhs = mk_f_exists src_evs (mk_f_array src_array) in
    mk_entailment pstate.pgs_formula rhs in
  (* let _ = nhdebugc "GetElemPtr: Entailment:\n" pr_ent ent in *)
  let res, frames = PV.infer_entailment_frame vstate.vrs_core_prog ent in
  (* let _ = hdebugc "==> Result: " pr_ternary res in *)
  (* let _ = hdebugc "==> Frame: " pr_fs frames in *)
  match res, frames with
  | Some true, [ frame ] ->
    let npstate = { pstate with pgs_formula = frame } in
    if check_buffer_overflow vstate instr npstate src src_size index
    then pstate
    else (
      let cell_value = fresh_exp_var ~name:"t" src_etyp in
      let cell = mk_data src_etyp dst [ cell_value ] ~addr:(Some dst_addr) in
      let dst_array = mk_f_array (update_array_form src_array [ cell ]) in
      let nf = mk_f_exists src_evs (mk_f_star_with frame ~fs:[ dst_array ]) in
      let nf = NO.simplify_f nf in
      (* TODO: need to update pstate *)
      { pstate with pgs_formula = nf })
  | _ ->
    let _ = warning "Fail to analyze instruction GetElemPtr" in
    pstate
;;

let process_getelemptr_struct vstate instr pstate src index field dst =
  let src_etyp = get_pointer_elem_typ (typ_of_exp src) in
  let src_size = mk_exp_var (fresh_new_var (TInt 32)) in
  let src_evs = fv_es [ src_size ] in
  let src_array, dst_addr =
    let addrs = find_addr_exp_of_exp pstate.pgs_formula src in
    match addrs with
    | [] ->
      herror "process_getelemptr_struct: struct root not found:" pr_exp src
    | [ a ] ->
      let src_size = src_size |> mk_add a.addr_elem |> NO.simplify_arith_e in
      let src_array = mk_array a.addr_base src_size src_etyp in
      let dst_index = index |> mk_add a.addr_elem |> NO.simplify_arith_e in
      let dst_addr = mk_addr a.addr_base dst_index (mk_exp_int 0) in
      src_array, dst_addr
    | _ -> herror "process_getelemptr: too many array cells of:" pr_exp src
  in
  (* let _ = nhdebugc "Src array: " pr_array_form src_array in *)
  let ent =
    let rhs = mk_f_exists src_evs (mk_f_array src_array) in
    mk_entailment pstate.pgs_formula rhs in
  (* let _ = nhdebugc "GetElemPtr: Entailment:\n" pr_ent ent in *)
  let res, frames = PV.infer_entailment_frame vstate.vrs_core_prog ent in
  (* let _ = nhdebugc "==> Result: " pr_ternary res in *)
  (* let _ = nhdebugc "==> Frame: " pr_fs frames in *)
  match res, frames with
  | Some true, [ frame ] ->
    let npstate = { pstate with pgs_formula = frame } in
    if check_buffer_overflow vstate instr npstate src src_size index
    then pstate
    else (
      let cell_value = fresh_exp_var ~name:"t" src_etyp in
      let cell = mk_data src_etyp dst [ cell_value ] ~addr:(Some dst_addr) in
      let dst_array = mk_f_array (update_array_form src_array [ cell ]) in
      let nf = mk_f_exists src_evs (mk_f_star_with frame ~fs:[ dst_array ]) in
      let nf = NO.simplify_f nf in
      (* TODO: need to update pstate *)
      { pstate with pgs_formula = nf })
  | _ ->
    let _ = warning "Fail to analyze instruction GetElemPtr" in
    pstate
;;

let analyze_instr_getelemptr vstate pstates (instr : LI.instr) =
  let src = translate_llvalue (LI.operand instr 0) in
  let _ = hdebugc "SRC:" pr_exp src in
  let elem = translate_llvalue (LI.operand instr 1) in
  let field = translate_llvalue (LI.operand instr 2) in
  let _ = hdebugc "GetElemPtr ELEM: " pr_exp elem in
  let _ = hdebugc "           FIELD: " pr_exp field in
  let dst = translate_instr instr in
  List.map
    ~f:(fun pstate ->
      let src_typ = typ_of_exp src in
      let _ = hdebugc "SRC_TYP: " pr_type src_typ in
      match src_typ with
      | TPointer (TStruct _) ->
        (* let _ = ndebugc "analyze_getelemptr of struct" in *)
        process_getelemptr_struct vstate instr pstate src elem field dst
      | TPointer TArray ->
        (* let _ = ndebugc "analyze_getelemptr of array" in *)
        process_getelemptr_array vstate instr pstate src elem field dst
      | _ -> error "analyze_getelemptr: expect source of pointer type")
    pstates
;;

(*** instruction trunc (casting) ***)

let analyze_instr_trunc vstate pstates (instr : LI.instr) =
  let src = translate_llvalue (LI.operand instr 0) in
  let dst = translate_instr instr in
  let pf = mk_eq dst src in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction zext (casting) ***)

let analyze_instr_zext vstate pstates (instr : LI.instr) =
  let src = translate_llvalue (LI.operand instr 0) in
  let dst = translate_instr instr in
  let pf = mk_eq dst src in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction sext (casting) ***)

let analyze_instr_sext vstate pstates (instr : LI.instr) =
  let src = translate_llvalue (LI.operand instr 0) in
  let dst = translate_instr instr in
  let pf = mk_eq dst src in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction bitcast (casting) ***)

let process_one_state_instr_bitcast vstate pstate instr src dst =
  (* let prog = vstate.vrs_prog in
   * (\* TODO: need to check whether src is an array or a struct? *\)
   * let src_asize = mk_exp_var (fresh_new_var (TInt 32)) in
   * let src_evs = fv_es [src_asize; src] in
   * let src_array =
   *   let f = mk_f_array (mk_array src src_asize src_ememtyp) in
   *   let size_cond = mk_gt src_asize (mk_exp_int 0) in
   *   mk_f_star_with f ~pfs:[size_cond] in
   * let ent = mk_entailment pstate.pgs_formula (mk_f_exists src_evs src_array) in
   * let _ = hdebugc "Bitcast Entailment:\n" pr_ent ent in
   * (\* TRUNG: possibly no need to check entailment for BitCast. *\)
   * let res, frames = PV.infer_entailment_frame prog ent in
   * let _ = hdebugc "==> Result: " pr_ternary res in
   * let _ = hdebugc "==> Frame: " pr_fs frames in
   * match res, frames with
   * | True, [frame] ->
   *   let is_casting_empty_memory =
   *     let pframe = NO.encode_formula_to_pure frame in
   *     Smt.check_imply pframe (mk_eq_exp_int src_asize 0) = True in
   *   if is_casting_empty_memory then
   *     let addr_pf = mk_eq dst src in
   *     let nf = mk_f_star_with frame ~pfs:[addr_pf] in
   *     let nf = NO.simplify_f (mk_f_exists src_evs nf) in
   *     {pstate with pgs_formula = nf}
   *   else
   *     let dst_etyp = typ_of_exp dst in
   *     let dst_lletyp = LL.element_type (LL.type_of instr) in
   *     let dst_lletyp_size = LD.abi_size dst_lletyp (LD.of_string !data_layout) in
   *     let dst_asize = mk_exp_var (fresh_new_var (TInt 32)) in
   *     let dst_evs = fv_e dst_asize in
   *     let dst_array = mk_array dst dst_asize dst_etyp in
   *     let _ = hdebugc "SRC_MEMTYP: " pr_type src_memtyp in
   *     let _ = hdebugc "DST_LLETYP: " pr_type dst_lletyp in
   *     let size_pf =
   *       let src_memtyp_size = mk_exp_int (Int64.to_int_exn src_memtyp_size) in
   *       let dst_etyp_size = Int64.to_int_exn dst_lletyp_size in
   *       let _ = hdebugc "src_memtyp_size: " pr_exp src_memtyp_size in
   *       let _ = hdebugc "dst_lletyp_size: " string_of_int dst_etyp_size in
   *       let dst_size = mk_mul_exp_int dst_asize dst_etyp_size in
   *       let _ = hdebugc "dst_size: " pr_exp dst_size in
   *       mk_eq src_memtyp_size dst_size in
   *     let addr_pf = mk_eq dst src in
   *     let nf = mk_f_star_with frame ~afs:[dst_array] ~pfs:[size_pf; addr_pf] in
   *     (\* let nf = mk_f_star_with frame ~afs:[dst_array] ~pfs:[size_pf] in *\)
   *     let nf = mk_f_exists (src_evs @ dst_evs) nf in
   *     let nf = NO.simplify_f nf in
   *     {pstate with pgs_formula = nf}
   * | _ ->
   *   let _ = warning "Fail to analyze BitCast Instr" in
   *   pstate *)
  pstate
;;

let analyze_instr_bitcast vstate pstates (instr : LI.instr) =
  let src = translate_llvalue (LI.operand instr 0) in
  let dst = translate_instr instr in
  let src_mem_size =
    let src_mem_lltyp = LL.element_type (LL.type_of (LI.operand instr 0)) in
    let size = LTD.abi_size src_mem_lltyp (LTD.of_string !data_layout) in
    mk_exp_int (Int64.to_int_exn size) in
  List.map
    ~f:(fun ps -> process_one_state_instr_bitcast vstate ps instr src dst)
    pstates
;;

(*** instruction icmp ***)

let analyze_instr_icmp vstate pstates (instr : LI.instr) =
  let exp0 = translate_llvalue (LI.operand instr 0) in
  let exp1 = translate_llvalue (LI.operand instr 1) in
  let cmp =
    match LI.predicate_of_instr_icmp instr with
    | None -> error "analyze_instr_icmp: comparison predicate not found"
    | Some p -> mk_preln (translate_ll_icmp p) [ exp0; exp1 ] in
  let dst = translate_instr instr in
  let pf = mk_beq dst cmp in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction fcmp ***)

let analyze_instr_fcmp vstate pstates (instr : LI.instr) =
  let exp0 = translate_llvalue (LI.operand instr 0) in
  let exp1 = translate_llvalue (LI.operand instr 1) in
  let cmp =
    match LI.predicate_of_instr_fcmp instr with
    | None -> error "analyze_instr_fcmp: comparison predicate not found"
    | Some p -> mk_preln (translate_ll_fcmp p) [ exp0; exp1 ] in
  let dst = translate_instr instr in
  let pf = mk_beq dst cmp in
  mk_star_program_states pstates ~pfs:[ pf ]
;;

(*** instruction call ***)

let process_one_state_instr_call vstate instr pstate func args return =
  let precond, postcond = get_prepost_llproc func args return in
  let proc_name = LI.func_name func in
  let evs = diff_vs (fv_f precond) (fv_es args) in
  let ent = mk_entailment pstate.pgs_formula (mk_f_exists evs precond) in
  let _ = hdebugc "CALL: Prove Pre-Condition By Frame:\n\n" pr_ent ent in
  let res, frames = PV.infer_entailment_frame vstate.vrs_core_prog ent in
  let _ = hdebugc "==> Result: " pr_bresult res in
  let _ = hdebugc "==> Frame: " pr_fs frames in
  match res, frames with
  | Some true, [ frame ] ->
    let nf = mk_f_exists evs (mk_f_star_with frame ~fs:[ postcond ]) in
    let nf = NO.simplify_f nf in
    let npstate = { pstate with pgs_formula = nf } in
    let npstate =
      if LI.is_func_free func
      then (
        let _ = hdebugc " Freed args: " pr_exps args in
        let freed_exps = get_original_freed_pointers npstate args in
        let _ = hdebugc " Freed exps: " pr_exps freed_exps in
        propagate_program_state_freed_pointers npstate freed_exps)
      else npstate in
    (* TODO: update alloced and freed data *)
    npstate
  | Some true, _ ->
    herror "process_instr_call: expect 1 frame but found: " pr_fs frames
  | _ ->
    let buggy = check_double_free vstate instr pstate func args in
    if buggy then { pstate with pgs_is_buggy = true } else pstate
;;

let analyze_instr_call vstate pstates (instr : LI.instr) =
  (* compute with pre *)
  let num_operand = LI.num_operands instr in
  let func = LI.callee_of_instr_call instr in
  match LI.is_func_llvm_debug func with
  | true -> pstates
  | false ->
    let return = translate_instr instr in
    let args = get_operands (LI.llvalue_of_instr instr) 0 (num_operand - 2) in
    List.map
      ~f:(fun pstate ->
        process_one_state_instr_call vstate instr pstate func args return)
      pstates
;;

(*******************************************************************
 ** analyze instruction
 *******************************************************************)

let analyze_instruction vstate pstates (instr : LI.instr) =
  let op = LI.instr_opcode instr in
  (* analyze states *)
  let pstates =
    match op with
    | OC.Ret -> analyze_instr_return vstate pstates instr
    | OC.Br -> analyze_instr_br vstate pstates instr
    | OC.Unreachable -> analyze_instr_unreachable vstate pstates instr
    | OC.Add
    | OC.FAdd
    | OC.Sub
    | OC.FSub
    | OC.Mul
    | OC.FMul
    | OC.UDiv
    | OC.SDiv
    | OC.FDiv -> analyze_instr_arith vstate pstates instr
    | OC.Alloca -> analyze_instr_alloca vstate pstates instr
    | OC.Load -> analyze_instr_load vstate pstates instr
    | OC.Store -> analyze_instr_store vstate pstates instr
    | OC.GetElementPtr -> analyze_instr_getelemptr vstate pstates instr
    | OC.Trunc -> analyze_instr_trunc vstate pstates instr
    | OC.ZExt -> analyze_instr_zext vstate pstates instr
    | OC.SExt -> analyze_instr_sext vstate pstates instr
    | OC.BitCast -> analyze_instr_bitcast vstate pstates instr
    | OC.ICmp -> analyze_instr_icmp vstate pstates instr
    | OC.FCmp -> analyze_instr_fcmp vstate pstates instr
    | OC.Call -> analyze_instr_call vstate pstates instr
    | _ -> herror "analyze_instruction: need to handle" LI.pr_instr instr in
  (* update program states *)
  List.map ~f:(fun ps -> { ps with pgs_last_instr = Some instr }) pstates
;;

(*******************************************************************
 ** symbolic execution
 *******************************************************************)

let symexec_block vstate (blk : LI.block) : entailment list =
  let prog = vstate.vrs_llvm_prog in
  debugc ("Analyzing Block: " ^ LI.block_name blk ^ ":\n" ^ LI.pr_block blk);
  let predecessors = LI.get_preceding_blocks prog blk in
  let vf_blk = encode_block_pre_state blk in
  let init_entails =
    match predecessors with
    | [] ->
      let proc_pred = encode_precond_llproc (LI.func_of_block blk) in
      [ mk_entailment (f_of_view proc_pred) (f_of_view vf_blk) ]
    | _ -> [] in
  let init_states = [ mk_program_state (f_of_view vf_blk) init_entails ] in
  let post_states =
    LI.fold_left_instrs
      ~f:(fun st instr ->
        let _ = print_endline ("\n" ^ pr_program_states st ^ "\n") in
        let _ = print_endline ("++" ^ LI.pr_instr instr) in
        analyze_instruction vstate st instr)
      ~init:init_states
      blk in
  let _ = print_endline ("\n" ^ pr_program_states post_states) in
  let final_states =
    List.map
      ~f:(fun ps ->
        let hbody = ps.pgs_formula in
        let hhead =
          match ps.pgs_next_block with
          | None -> encode_postcond_llproc (LI.func_of_block blk)
          | Some nblk -> encode_block_pre_state nblk in
        let ent = mk_entailment hbody (f_of_view hhead) in
        propagate_program_state_entails ps [ ent ])
      post_states in
  let all_entails =
    List.fold ~f:(fun acc s -> acc @ s.pgs_entails) ~init:[] final_states in
  let _ =
    debugc
      ("--------\nEntailments: "
      ^ LI.block_name blk
      ^ ": \n"
      ^ pr_ents all_entails
      ^ "\n") in
  all_entails
;;

let symexc_function vstate (func : LI.func) : entailment list =
  if List.is_empty (LI.blocks_of_func func)
  then []
  else (
    hdebugc "Analyzing function: " LI.func_name func;
    LI.fold_left_blocks
      ~f:(fun acc blk -> acc @ symexec_block vstate blk)
      ~init:[]
      func)
;;

let symexec_program vstate (prog : LI.program) : entailment list =
  (* user functions *)
  let ents =
    List.fold_left
      ~f:(fun acc f -> acc @ symexc_function vstate f)
      ~init:[]
      prog.prog_user_funcs in
  debugc
    ("=======================================\n"
    ^ "ALL ENTAILMENTS:\n"
    ^ pr_ents ents);
  let _ = Export.Entails.export_entailments ~export:!export_entailment ents in
  ents
;;

(*******************************************************************
 ** verification
 *******************************************************************)

let verify_program (prog : LI.program) =
  let _ = data_layout := prog.prog_data_layout in
  let vstate = mk_verifier_state prog in
  let ents = symexec_program vstate prog in
  ()
;;
