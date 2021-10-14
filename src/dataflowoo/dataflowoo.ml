(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore
open Llir

module AS = Assertion
module LL = Llvm
module LV = Llvm.ValueKind
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LP = Llloop
module LG = Llcallgraph
module BG = Bug
module SP = Set.Poly

type 't callsite = {
  cs_instr_call : instr;
  cs_caller : func;
  cs_caller_input : 't;
  cs_caller_callsites : 't callsite list;
  cs_callee : func;
  cs_callee_input : 't;
}

type 't callsites = 't callsite list

type 't exn = {
  exn_orig_expr : expr;
  exn_root_expr : expr;
  exn_data : 't;
  exn_type_info : llvalue;
}

type 't exns = 't exn list

type 't working_block = {
  wb_block : block;
  wb_instr : instr;
  wb_instr_input : 't;
}

type 't working_func = {
  wf_callsites: 't callsite list;
  wf_func: func;
  wf_input: 't;
}

type 't global_env = {
  genv_global_output : (global, 't) Hashtbl.t;
  mutable genv_globals_data : 't;
}

type 't func_env = {
  fenv_id : string;
  fenv_func : func;
  fenv_callsites : 't callsite list;
  fenv_prog : program;
  fenv_instr_output : (instr, 't) Hashtbl.t;
  fenv_block_input : (block, 't) Hashtbl.t;
  mutable fenv_input : 't;                        (* arguments and globals *)
  (* TODO: maybe adding location of this input (func call) for debugging? *)
  mutable fenv_output : 't option;
  fenv_thrown_exn : (llvalue, 't exn) Hashtbl.t;
  fenv_landing_exns : (llvalue, 't exn list) Hashtbl.t;
  mutable fenv_deref_params : params;
  mutable fenv_deref_globals : globals;
  mutable fenv_working_blocks : 't working_block list; (* block an instruction to be analyzed *)
  mutable fenv_state_changed : bool;
}

type 't func_summary = {
  fsum_func : func;
  fsum_input : 't;                        (* data of globals and params *)
  fsum_output : 't;                       (* data of globals and returned *)
  fsum_thrown_exn : 't exn list;
  fsum_deref_params : params;
  fsum_deref_globals : globals;
}

type 't prog_env = {
  penv_prog : program;
  penv_global_env : 't global_env;

  (* TODO: also need to capture different summaries *)
  penv_func_envs : (func, 't func_env list) Hashtbl.t;
  penv_func_summaries : (func, 't func_summary list) Hashtbl.t;

  (* possible inputs of a function, consider both globals and params *)
  penv_func_analyzed_inputs : (func, 't list) Hashtbl.t;

  (* candidate functions to be analyzed *)
  mutable penv_goal_funcs : funcs;                    (* all candidate functions *)
  mutable penv_working_funcs : 't working_func list;  (* functions to be analyzed*)

  (* sparse analysis *)
  penv_sparse_llvalue : (llvalue, bool) Hashtbl.t;
  penv_sparse_block : (block, bool) Hashtbl.t;
  penv_sparse_func : (func, bool) Hashtbl.t;
  penv_sparse_used_globals: (func, globals) Hashtbl.t;
  penv_block_sparse_instrs : (block, instr list) Hashtbl.t;

  (* mapping a block to its preceding and succeeding blocks *)
  penv_sparse_precedings_block : (block, block list) Hashtbl.t;
  penv_sparse_succeedings_block : (block, block list) Hashtbl.t;
  penv_sparse_reachable_blocks : (block, block list) Hashtbl.t;

  (* analysis statistics *)
  penv_func_analyzed_times : (func, int) Hashtbl.t;
  penv_block_local_analyzed_times : (block, int) Hashtbl.t;
  penv_block_total_analyzed_times : (block, int) Hashtbl.t;
  penv_func_analysis_stack : func Stack.t;
  penv_block_analyzed_squence : (func, blocks) Hashtbl.t;
}


class ['t] data = object(self)
  method get_global_output (genv: 't global_env) global : 't option =
    Hashtbl.find genv.genv_global_output global

  method set_global_output (genv: 't global_env) global (data: 't) : unit =
    Hashtbl.set genv.genv_global_output ~key:global ~data

  (* get and set instructions' output *)

  method get_instr_output (fenv: 't func_env) (instr: instr) : 't option =
    Hashtbl.find fenv.fenv_instr_output instr

  method set_instr_output (fenv: 't func_env) instr (data: 't) : unit =
    Hashtbl.set fenv.fenv_instr_output ~key:instr ~data

  (* get and set blocks' output *)

  method get_block_input (fenv: 't func_env) blk : 't option =
    Hashtbl.find fenv.fenv_block_input blk

  method set_block_input (fenv: 't func_env) blk (input: 't) : unit =
    Hashtbl.set fenv.fenv_block_input ~key:blk ~data:input

  (* default pre- and post-analysis functions *)

  method init_sparse_globals_instrs (penv: 't prog_env) : unit =
    let fglobal = Some (fun g ->
        let vg = llvalue_of_global g in
        Hashtbl.set penv.penv_sparse_llvalue ~key:vg ~data:true) in
    let finstr = Some (fun i ->
        let vi = llvalue_of_instr i in
        Hashtbl.set penv.penv_sparse_llvalue ~key:vi ~data:true) in
    deep_iter_program ~fglobal ~finstr penv.penv_prog

  method refine_sparse_globals_instrs (penv: 't prog_env) : bool = false

  method pre_analyze_func (penv: 't prog_env) (fenv: 't func_env) : unit = ()

  method post_analyze_func (penv: 't prog_env) (fenv: 't func_env) : unit = ()

  method pre_analyze_prog (penv: 't prog_env) : unit = ()

  method post_analyze_prog (penv: 't prog_env) : unit = ()

  (* sparse analysis *)

  method is_sparse_llvalue (penv: 't prog_env) (v: llvalue) : bool =
    if !dfa_sparse_analysis then
      match Hashtbl.find penv.penv_sparse_llvalue v with
      | None -> false
      | Some b -> b
    else true

  method is_sparse_global (penv: 't prog_env) (g: global) : bool =
    self#is_sparse_llvalue penv (llvalue_of_global g)

  method is_sparse_instr (penv: 't prog_env) (i: instr) : bool =
    self#is_sparse_llvalue penv (llvalue_of_instr i)

  method is_sparse_block (penv: 't prog_env) blk : bool =
    if !dfa_sparse_analysis then
      match Hashtbl.find penv.penv_sparse_block blk with
      | Some res -> res
      | None -> false
    else true

  method is_sparse_func (penv: 't prog_env) func : bool =
    if !dfa_sparse_analysis then
      match Hashtbl.find penv.penv_sparse_func func with
      | Some res -> res
      | None -> false
    else true

  method get_sparse_used_globals (penv: 't prog_env) func : globals =
    if !dfa_sparse_analysis then
      match Hashtbl.find penv.penv_sparse_used_globals func with
      | None -> []
      | Some gs -> gs
    else get_func_used_globals penv.penv_prog func

end

class virtual ['t] forward_data_transfer dfa = object(self)
  val analysis : dfa_analysis = dfa

  (*-----------------------------------------
   * Handling abstract data
   *-----------------------------------------*)
  val virtual least_data : 't

  method virtual pr_data : 't -> string
  method virtual pr_data_checksum : 't -> string
  method virtual equal_data : 't -> 't -> bool
  method virtual lequal_data : 't -> 't -> bool
  method virtual copy_data : 't -> 't
  method virtual subst_data : ?sstv:substv -> ?sstve:substve -> ?sste:subste -> 't -> 't
  method virtual merge_data : ?widen:bool -> 't -> 't -> 't
  method virtual join_data : 't -> 't -> 't

  (*-----------------------------------------
   * Core analysis functions
   *-----------------------------------------*)

  method virtual need_widening : func -> bool
  method virtual clean_irrelevant_info_from_data : 't prog_env -> func -> 't -> 't
  method virtual clean_info_of_vars : 't -> llvalues -> 't
  method virtual is_data_satisfied_predicate : 't -> predicate -> bool
  method virtual refine_data_by_predicate : ?widen:bool -> 't -> predicate -> 't
  method virtual prepare_callee_input :
    't prog_env -> instr -> func -> llvalue list -> 't -> 't
  method virtual compute_callee_output_exns :
    't prog_env -> instr -> func -> llvalue list -> 't -> 't func_summary -> 't * ('t exns)
  method virtual prepare_thrown_exception_data : 't prog_env -> llvalue -> llvalue -> 't -> 't
  method virtual compute_catch_exception_data : 't prog_env -> instr -> llvalue -> 't -> 't exn -> 't
  method virtual analyze_global : global -> 't -> 't
  method virtual analyze_instr : ?widen:bool -> 't prog_env -> 't func_env -> instr -> 't -> 't

  (*-----------------------------------------
   * Handling bug and assertions
   *-----------------------------------------*)

  (** Check and return if a potential bug is a real bug *)
  method virtual check_bug : 't func_env -> BG.bug -> ternary

  (** Count the number of assertions relevant to an analysis *)
  method virtual count_assertions : program -> int

  (** Return the number of checked assertions *)
  method virtual check_func_assertions: 't prog_env -> func -> int

end


(*******************************************************************
 ** ForwardDataFlow Analysis
 *******************************************************************)

class virtual ['t] forward_data_flow dfa = object(self)
  inherit ['t] data
  inherit ['t] forward_data_transfer dfa

  (* sparse analysis *)

  method get_sparse_instrs_in_block ?(cache=true) (penv : 't prog_env) blk : instrs =
    let compute blk =
      fold_left_instrs ~f:(fun acc instr ->
        if self#is_sparse_instr penv instr then acc @ [instr]
        else acc) ~init:[] blk in
    if cache then
      Hashtbl.find_or_compute penv.penv_block_sparse_instrs ~key:blk
        ~f:(fun () -> compute blk)
    else compute blk

  method get_sparse_preceding_blocks ?(cache=true) (penv : 't prog_env) blk : block list =
    let rec compute_blocks blks visited res : block list =
      match blks with
      | [] -> res
      | blk::nblks ->
        let equal = equal_block in
        let pblks = get_preceding_blocks penv.penv_prog blk in
        let transfer_blks, target_blks =
          List.fold_left
            ~f:(fun (acctf, acctg) pblk ->
                 let pblk = pblk.pblk_block in
                 let sparse_instrs = self#get_sparse_instrs_in_block ~cache penv pblk in
                 if List.length sparse_instrs = 0 then (acctf, acctg)
                 else if List.for_all ~f:is_instr_br_or_switch sparse_instrs then
                   (List.append_dedup acctf pblk ~equal, acctg)
                 else (acctf, List.append_dedup acctg pblk ~equal)
               ) ~init:([], []) pblks in
        let nvisited = List.append_dedup visited blk ~equal:equal_block in
        let nblks =
          let tblks = List.filter ~f:(fun b ->
                                       List.not_mem visited b ~equal:equal_block) transfer_blks in
          List.concat_dedup nblks tblks ~equal:equal_block in
        let nres = List.concat_dedup res target_blks ~equal:equal_block in
        compute_blocks nblks nvisited nres in
    let compute blk =
      if !dfa_sparse_analysis then
        let blks = compute_blocks [blk] [] [] in
        let blks = List.sorti ~compare:compare_block_by_name blks in
        List.filter ~f:(fun b -> not (equal_block blk b)) blks
      else
        let pblks = get_preceding_blocks penv.penv_prog blk in
        List.map ~f:block_of_prec_block pblks in
    if cache then
      Hashtbl.find_or_compute penv.penv_sparse_precedings_block ~key:blk
        ~f:(fun () -> compute blk)
    else compute blk

  method get_sparse_succeeding_blocks ?(cache=true) (penv : 't prog_env) blk : block list =
    (* let _ = hdebug "get_sparse_succeeding_blocks of: " block_name blk in *)
    let prog = penv.penv_prog in
    let rec compute_blocks blks visited res : block list =
      let equal = equal_block in
      match blks with
      | [] -> res
      | blk::nblks ->
          (* let _ = hdebug " current block: " block_name blk in *)
          let sblks = get_succeeding_blocks prog blk in
          let transfer_blks, target_blks =
            List.fold_left
              ~f:(fun (acctf, acctg) sblk ->
                   let sblk = sblk.sblk_block in
                   let sparse_instrs = self#get_sparse_instrs_in_block ~cache penv sblk in
                   let ssblks = get_succeeding_blocks prog sblk in
                   if List.length sparse_instrs = 0 then (acctf, acctg)
                   else if List.for_all ~f:is_instr_br_or_switch sparse_instrs &&
                           List.not_empty ssblks then
                     (List.append_dedup acctf sblk ~equal, acctg)
                   else (acctf, List.append_dedup acctg sblk ~equal))
              ~init:([], [])
              sblks in
          let nvisited = List.append_dedup visited blk ~equal:equal_block in
          let nblks =
            let tblks = List.filter ~f:(fun b ->
                List.not_mem visited b ~equal) transfer_blks in
            List.concat_dedup nblks tblks ~equal in
          let nres = List.concat_dedup res target_blks ~equal in
          compute_blocks nblks nvisited nres in
    let compute blk =
      if !dfa_sparse_analysis then
        let blks = compute_blocks [blk] [] [] in
        let blks = List.sorti ~compare:compare_block_by_name blks in
        List.filter ~f:(fun b -> not (equal_block blk b)) blks
      else
        let sblks = get_succeeding_blocks prog blk in
        List.map ~f:block_of_succ_block sblks in
    if cache then
      Hashtbl.find_or_compute penv.penv_sparse_succeedings_block ~key:blk
        ~f:(fun () -> compute blk)
    else compute blk

  method get_sparse_reachable_blocks (penv : 't prog_env) (blk: block) : blocks =
    let rec compute_reachables (queue: blocks) (visited: blocks) =
      match queue with
      | [] -> visited
      | blk::nqueue ->
          let nblks = blk |> self#get_sparse_succeeding_blocks penv |>
                      List.exclude ~f:(List.mem ~equal:(==) visited) |>
                      List.exclude ~f:(List.mem ~equal:(==) nqueue) in
          let nqueue = nqueue @ nblks in
          let nvisited = visited @ [blk] in
          compute_reachables nqueue nvisited in
    let compute () =
      let sblks = self#get_sparse_succeeding_blocks penv blk in
      compute_reachables sblks [] in
    Hashtbl.find_or_compute penv.penv_sparse_reachable_blocks ~key:blk ~f:compute


  method is_sparse_reachable_block (penv : 't prog_env) (src: block) (dst: block) : bool =
    let func1, func2 = func_of_block src, func_of_block dst in
    if equal_func func1 func2 then
      let blks = self#get_sparse_reachable_blocks penv src in
      List.mem blks dst ~equal:equal_block
    else false


  (* printing *)

  method pr_data_with_checksum (d: 't) =
    (self#pr_data_checksum d) ^ "\n" ^
    (self#pr_data d)

  method pr_data (d: 't) =
    if !print_concise_output && not (is_debug_mode ()) then self#pr_data_checksum d
    else self#pr_data d
  (* (pr_data_checksum d) ^ "\n" ^
   * (pr_data d) *)

  method pr_datas ?(bullet="-") (ds: 't list) : string =
    pr_items ~bullet self#pr_data ds

  method pr_data_opt (d: 't option) : string =
    match d with
    | None -> "{No data}"
    | Some d -> self#pr_data d

  method pr_prog_globals (penv: 't prog_env) : string =
    let prog = penv.penv_prog in
    let genv = penv.penv_global_env in
    if List.is_empty prog.prog_globals then ""
    else
      let ginput = (hpr_indent 4 self#pr_data least_data) in
      let globals = List.fold_left ~f:(fun acc global ->
          acc ^ "\n  " ^ (pr_global global) ^ "\n" ^
          (hpr_indent 4 self#pr_data_opt (self#get_global_output genv global))
        ) prog.prog_globals ~init:ginput in
      "\n\nGlobals:\n\n" ^ globals

  method pr_func_output (output: 't * blocks) : string =
    self#pr_data (fst output)

  method pr_callsite (cs: 't callsite) : string =
    let caller, instr = cs.cs_caller, cs.cs_instr_call in
    let blk = block_of_instr instr in
    let index =
      try
        let _ = fold_left_instrs ~f:(fun acc i ->
            if equal_instr instr i then raise (EInt acc)
            else acc + 1) ~init:1 blk in
        -1
      with (EInt res) -> res in
    (func_name caller) ^ ":" ^ (block_name blk) ^ ":" ^ (pr_int index)

  method pr_callsites (css: 't callsite list) : string =
    pr_list ~sep:",\n" self#pr_callsite css

  method pr_exn (exn: 't exn) : string =
    "Exception:\n" ^
    "  Pointer: " ^ (pr_expr exn.exn_orig_expr) ^ "\n" ^
    "  Data: " ^ (self#pr_data exn.exn_data) ^ "\n" ^
    "  Type info: " ^ (pr_value exn.exn_type_info)

  method pr_exns (exns: 't exn list) : string =
    pr_items self#pr_exn exns

  method pr_working_block (wb: 't working_block) : string =
    (block_name wb.wb_block) ^ " @ " ^
    (pr_instr wb.wb_instr) ^ " @ {" ^
    (self#pr_data wb.wb_instr_input) ^ " }"

  method pr_working_blocks (wbs: 't working_block list) : string =
    pr_items ~bullet:" +" self#pr_working_block wbs

  method pr_working_func (wf: 't working_func) : string =
    let _ = if !mode_debug_working_function then
        let regex = Str.regexp !regex_debug_working_function in
        if Str.string_match regex (func_name wf.wf_func) 0 then
          let _ = save_mode_debug () in
          enable_mode_debug () in
    let input =
      let str = self#pr_data wf.wf_input in
      if String.is_substring str ~substring:"\n" then
        str |> String.split_lines |> List.map ~f:(fun s -> "\n  " ^ s) |>
        String.concat ~sep:""
      else str in
    let res = (func_name wf.wf_func) ^ " @ " ^
              (pr_list self#pr_callsite wf.wf_callsites) ^ " @ {" ^
              input ^ "}" in
    let _ = if !mode_debug_working_function then
        restore_mode_debug () in
    res

  method pr_working_funcs (wfs: 't working_func list) : string =
    pr_items self#pr_working_func wfs

  method pr_analyzed_func_input (penv : 't prog_env) : string =
    let func_inputs = Hashtbl.fold ~f:(fun ~key:f ~data:inputs acc ->
        let sfuncs = List.map ~f:(fun input ->
            (func_name f) ^ " @ {" ^ (self#pr_data input) ^ "}") inputs in
        acc @ sfuncs) ~init:[] penv.penv_func_analyzed_inputs in
    pr_items ~bullet:"+" pr_id func_inputs

  method pr_func_summary (fsum: 't func_summary) : string =
    let fname_params = func_name_and_params fsum.fsum_func in
    let deref_params_globals =
      let vs = (fsum.fsum_deref_params |> llvalues_of_params) @
               (fsum.fsum_deref_globals |> llvalues_of_globals) in
      pr_list pr_value vs in
    "Function summary: " ^ fname_params ^ "\n" ^
    (hpr_align "- Input:  " self#pr_data fsum.fsum_input) ^ "\n" ^
    (hpr_align "- Output: " self#pr_data fsum.fsum_output) ^ "\n" ^
    (pr_align "- Deref params and globals: " deref_params_globals)

  method pr_analysis_stats (penv: 't prog_env) : string =
    let tbl_func_stats = penv.penv_func_analyzed_times in
    let tbl_block_stats = penv.penv_block_total_analyzed_times in
    let tbl_func_inputs = penv.penv_func_analyzed_inputs in
    let func_stats = List.map ~f:(fun func ->
        let ftimes = Hashtbl.find_default tbl_func_stats func ~default:0 in
        let num_inputs =
          let inputs = Hashtbl.find_default tbl_func_inputs func ~default:[] in
          List.length inputs in
        let func_stat = "  " ^ (func_name func) ^ ": " ^ (pr_int ftimes) ^
                        " times on " ^ (pr_int num_inputs) ^ " inputs" in
        if ftimes = 0 then func_stat
        else
          let blk_stats =
            let stats = map_blocks ~f:(fun blk ->
                let btimes = Hashtbl.find_default tbl_block_stats blk ~default:0 in
                (block_name blk) ^ ": " ^ (pr_int btimes)) func in
            beautiful_concat ~column:75 ~sep:", " stats in
          func_stat ^ "\n" ^ (pr_indent 4 blk_stats))
        penv.penv_goal_funcs in
    let func_sequence =
      let funcs = List.rev (Stack.to_list penv.penv_func_analysis_stack) in
      let fnames = List.map ~f:func_name funcs in
      pr_align "  " (beautiful_concat ~sep:", " fnames) in
    let total_func_analyzed_times = Stack.length penv.penv_func_analysis_stack in
    let func_stats = String.concat ~sep:"\n" func_stats in
    "- Analyzed times of functions and blocks:\n" ^ func_stats ^ "\n" ^
    "- Function analysis stack:\n" ^ func_sequence ^ "\n" ^
    "- Total functions analyzed times: " ^ (pr_int total_func_analyzed_times)

  method pr_sparse_instr (penv : 't prog_env) instr =
    if !dfa_sparse_analysis then
      let blk = block_of_instr instr in
      match instr_opcode instr with
      | LO.IndirectBr | LO.Br | LO.Switch ->
          "jump<sparse> " ^ (block_names (self#get_sparse_succeeding_blocks penv blk))
      | LO.Invoke ->
          (pr_instr instr) ^ ", jump<sparse> " ^
          (block_names (self#get_sparse_succeeding_blocks penv blk))
      | _ -> pr_instr instr
    else pr_instr instr

  method pr_func_env ?(id="") ?(sparse=true) (penv : 't prog_env) (fenv: 't func_env) : string =
    let prog, func = fenv.fenv_prog, fenv.fenv_func in
    let fname_params = func_name_and_params func in
    let sblocks = fold_left_blocks ~f:(fun acc1 blk ->
        if not sparse || self#is_sparse_block penv blk then
          let bname = block_name blk in
          let binput = hpr_indent 4 self#pr_data_opt (self#get_block_input fenv blk) in
          let instrs_output = fold_left_instrs ~f:(fun acc2 instr ->
              if not sparse || self#is_sparse_instr penv instr then
                acc2 ^ "\n\n  " ^ (self#pr_sparse_instr penv instr) ^ "\n\n" ^
                (match self#get_instr_output fenv instr with
                 | None -> pr_indent 4 "{No data}"
                 | Some output -> hpr_indent 4 self#pr_data output)
              else acc2) ~init:"" blk in
          acc1 ^ "\n\n " ^ bname ^ ":\n\n" ^ binput ^
          instrs_output ^ "\n\n -----"
        else acc1) ~init:"" func in
    let id =
      if String.is_empty id then id
      else " ~~ (" ^ id ^ ")" in
    "Function env: " ^ fname_params ^ id ^ "\n\n" ^
    " Callsite: " ^ (self#pr_callsites fenv.fenv_callsites) ^ "\n\n" ^
    (hpr_align " Input: " self#pr_data fenv.fenv_input) ^
    sblocks

  method pr_prog_env (penv: 't prog_env) : string =
    let prog = penv.penv_prog in
    let analyzed_funcs = penv.penv_goal_funcs in
    let sfuncs = List.fold_left ~f:(fun acc func ->
        match Hashtbl.find penv.penv_func_envs func with
        | Some fenvs ->
            let num_fenvs = pr_int (List.length fenvs) in
            let sfenvs = fenvs |>
                         List.map ~f:(fun fenv ->
                             let id = fenv.fenv_id ^ "/" ^ num_fenvs in
                             "=============\n\n " ^
                             (self#pr_func_env ~id penv fenv)) |>
                         String.concat ~sep:"\n\n" in
            acc ^ "\n\n" ^ sfenvs
        | None -> acc) ~init:"" analyzed_funcs in
    "Input program: " ^ (prog.prog_source_filename ) ^
    (self#pr_prog_globals penv) ^ sfuncs

  method pr_sparse_prog (penv: 't prog_env) : string =
    let pr_block (blk: block) : string =
      let blkname = block_name blk in
      let sinstrs = blk |>
                    fold_left_instrs ~f:(fun acc i ->
                        if self#is_sparse_instr penv i then
                          acc @ [hpr_indent 2 (self#pr_sparse_instr penv) i]
                        else if is_instr_invoke i then
                          let sblks = self#get_sparse_succeeding_blocks penv blk in
                          let jump_sparse = "jump<sparse> " ^ (block_names sblks) in
                          acc @ [pr_indent 2 jump_sparse]
                        else acc) ~init:[] |>
                    String.concat ~sep:"\n" in
      " " ^ blkname ^ ":\n" ^
      (String.replace_if_empty sinstrs ~replacer:"  {Empty block}") in
    let pr_func (f: func) : string =
      let fname = "Function: " ^ (pr_type (func_return_type f)) ^ " " ^
                  (func_name f) ^ "(" ^ (pr_args pr_param (func_params f)) ^ ")" in
      let sblks = f |> map_blocks ~f:pr_block |>
                  String.concat ~sep:"\n\n" |>
                  String.replace_if_empty ~replacer:" {Empty function}" in
      fname ^ "\n" ^ sblks in
    let prog = penv.penv_prog in
    let sglobals = prog.prog_globals |>
                   List.fold_left ~f:(fun acc g ->
                       if self#is_sparse_global penv g then
                         acc @ [hpr_indent 2 (pr_global ~detailed:true) g]
                       else acc) ~init:[] |>
                   String.concat ~sep:"\n" |>
                   String.prefix_if_not_empty ~prefix:"Globals:\n" in
    let sstructs = prog.prog_struct_types |>
                   List.map ~f:(fun t -> "  " ^ (pr_type t)) |>
                   String.concat ~sep:"\n" |>
                   String.prefix_if_not_empty ~prefix:"Struct types:\n" in
    let sfuncs =
      penv.penv_goal_funcs |> List.map ~f:pr_func |> String.concat ~sep:"\n\n" in
    (String.suffix_if_not_empty sglobals ~suffix:"\n\n") ^
    (String.suffix_if_not_empty sstructs ~suffix:"\n\n") ^
    sfuncs

  method print_stats_sparse_prog (penv : 't prog_env) : unit =
    (* hello *)
    let prog = penv.penv_prog in
    let num_struct_vars = ref 0 in
    let num_array_vars = ref 0 in
    let num_pointer_vars = ref 0 in
    let num_instrs = ref 0 in
    let num_blks = ref 0 in
    let num_user_funcs = ref 0 in
    let num_func_calls = ref 0 in
    let update_stats_of_llvalue v =
      let t = LL.type_of v in
      let users = get_users v in
      let _ = if is_type_array t then incr num_array_vars in
      let _ = if is_type_struct t then incr num_struct_vars in
      if is_type_pointer t then
        let elem_typ = LL.element_type t in
        let _ =
          if is_type_struct elem_typ then
            incr num_struct_vars
          else if is_type_array elem_typ then
            incr num_array_vars
          else if List.exists ~f:is_llvalue_instr_gep users then
            incr num_array_vars
          else () in
        incr num_pointer_vars in
    let fglobal = Some (fun global ->
        if self#is_sparse_global penv global then (
          let t = LL.element_type (type_of_global global) in
          let users = get_users (llvalue_of_global global) in
          let _ = if is_type_array t then incr num_array_vars in
          let _ = if is_type_struct t then incr num_struct_vars in
          if is_type_pointer t then
            let elem_typ = LL.element_type t in
            let _ =
              if is_type_struct elem_typ then
                incr num_struct_vars
              else if is_type_array elem_typ then
                incr num_array_vars
              else if List.exists ~f:is_llvalue_instr_gep users then
                incr num_array_vars
              else () in
            incr num_pointer_vars
          else ())
        else ()) in
    let fparam = Some (fun param ->
        let vparam = llvalue_of_param param in
        update_stats_of_llvalue vparam) in
    let finstr = Some (fun instr ->
        if self#is_sparse_instr penv instr then (
          let vinstr = llvalue_of_instr instr in
          let _ = update_stats_of_llvalue vinstr in
          let _ = if is_instr_call_invoke instr then incr num_func_calls in
          incr num_instrs)
        else ()) in
    let fblock = Some (fun blk ->
        let _ = incr num_blks in
        None) in
    let ffunc = Some (fun f ->
        if self#is_sparse_func penv f then
          let _ = incr num_user_funcs in
          None
        else Some ()) in
    let _ = deep_iter_program ~fglobal ~ffunc ~fparam ~fblock ~finstr prog in
    let stats =
      "\nSparse Pointer Statistics:\n" ^
      "  #Sparse User funcs: " ^ (pr_int !num_user_funcs) ^ "\n" ^
      "  #Sparse Blocks: " ^ (pr_int !num_blks) ^ "\n" ^
      "  #Sparse Instrs: " ^ (pr_int !num_instrs) ^ "\n" ^
      "  #Sparse Func calls: " ^ (pr_int !num_func_calls) ^ "\n" ^
      "  #Sparse Pointer Vars: " ^ (pr_int !num_pointer_vars) ^ "\n" ^
      "  #Sparse Struct Vars: " ^ (pr_int !num_struct_vars) ^ "\n" ^
      "  #Sparse Array Vars: " ^ (pr_int !num_array_vars) ^ "\n" in
    print ~format:false ~always:true stats

  method export_debugging_info_to_file (penv: 't prog_env) : unit =
    let prog = penv.penv_prog in
    let basefilename = Filename.chop_extension prog.prog_bitcode_filename in
    let pr_pretty_list printer items : string =
      let res = items |> List.map ~f:printer |>
                beautiful_concat ~sep:", " |>
                pr_indent ~skipfirst:true 4 in
      if String.is_empty res then " []"
      else "\n   [" ^ res ^ "]" in
    let _ =
      let filename = basefilename ^ ".globals.dbg" in
      let msg = print ("Export used globals information to: " ^ filename) in
      let file = open_out filename in
      let _ = fprintf file "===================================\n" in
      let _ = fprintf file "USED GLOBALS IN USER FUNCTIONS:\n\n" in
      let _ = List.iter ~f:(fun f ->
          let gs = f |> get_func_used_globals prog |>
                   List.sorti ~compare:compare_global_by_name in
          fprintf file "- %s: %s\n\n" (func_name f) (pr_pretty_list pr_global gs)
        ) penv.penv_goal_funcs in
      close_out file in
    let _ =
      let filename = basefilename ^ ".globals.sparse.dbg" in
      let msg = print ("Export used globals information to: " ^ filename) in
      let file = open_out filename in
      let _ = fprintf file "===================================\n" in
      let _ = fprintf file "USED SPARSE GLOBALS IN USER FUNCTIONS:\n\n" in
      let _ = List.iter ~f:(fun f ->
          let gs = f |> self#get_sparse_used_globals penv |>
                   List.sorti ~compare:compare_global_by_name in
          fprintf file "- %s: %s\n\n" (func_name f) (pr_pretty_list pr_global gs)
        ) penv.penv_goal_funcs in
      close_out file in
    let _ =
      let filename = basefilename ^ ".calleegraph.dbg" in
      let msg = print ("Export call graph information to: " ^ filename) in
      let file = open_out filename in
      let _ = fprintf file "===================================\n" in
      let _ = fprintf file "CALLEE GRAPH OF USER FUNCTIONS:\n\n" in
      let _ = List.iter ~f:(fun f ->
          let fs = f |> get_func_callees prog |> List.filter ~f:is_user_func in
          fprintf file "- %s:%s\n\n" (func_name f) (pr_pretty_list func_name fs)
        ) penv.penv_goal_funcs in
      close_out file in
    let _ =
      let filename = basefilename ^ ".callergraph.dbg" in
      let msg = print ("Export call graph information to: " ^ filename) in
      let file = open_out filename in
      let _ = fprintf file "===================================\n" in
      let _ = fprintf file "CALLER GRAPH OF USER FUNCTIONS:\n\n" in
      let _ = List.iter ~f:(fun f ->
          let fs = f |> get_func_callers prog |> List.filter ~f:is_user_func in
          fprintf file "- %s:%s\n\n" (func_name f) (pr_pretty_list func_name fs)
        ) penv.penv_goal_funcs in
      close_out file in
    let _ =
      let filename = basefilename ^ ".reachgraph.dbg" in
      let msg = print ("Export reachable graph information to: " ^ filename) in
      let file = open_out filename in
      let _ = fprintf file "===================================\n" in
      let _ = fprintf file "REACHABLE GRAPH OF USER FUNCTIONS:\n\n" in
      let _ = List.iter ~f:(fun f ->
          let fs = f |> get_reachable_funcs prog |> List.filter ~f:is_user_func in
          fprintf file "- %s:%s\n\n" (func_name f) (pr_pretty_list func_name fs)
        ) penv.penv_goal_funcs in
      close_out file in
    ()

  method export_core_program_to_file ?(sparse=false) (penv: 't prog_env) : unit =
    let prog = penv.penv_prog in
    let filename = (Filename.chop_extension prog.prog_bitcode_filename) ^
                   (if sparse then ".sparse.ll" else ".ll") in
    let msg = print ("Export " ^ (if sparse then "sparse " else "") ^
                     "program to: " ^ filename) in
    let file = open_out filename in
    let _ =
      let _ = fprintf file "Globals:\n" in
      List.iter ~f:(fun g ->
          if not sparse || self#is_sparse_global penv g then
            fprintf file "%s\n" (hpr_indent 2 (pr_global ~detailed:true) g)
        ) prog.prog_globals in
    let _ =
      let funcs = penv.penv_goal_funcs in
      List.iter ~f:(fun f ->
          let params = func_params f in
          let fname = "Function: " ^ (pr_type (func_return_type f)) ^ " " ^
                      (func_name f) ^ "(" ^ (pr_args pr_typed_param params) ^ ")" in
          let _ = fprintf file "\n\n%s\n" fname in
          iter_blocks ~f:(fun blk ->
              if not sparse || self#is_sparse_block penv blk then
                let _ = fprintf file "\n %s:\n" (block_name blk) in
                iter_instrs ~f:(fun instr ->
                    if not sparse then
                      fprintf file "%s\n" (hpr_indent 2 pr_instr instr)
                    else if self#is_sparse_instr penv instr then
                      fprintf file "  %s\n" (self#pr_sparse_instr penv instr)
                  ) blk
            ) f
        ) funcs in
    close_out file


  (* constructor *)

  method mk_callsite  ~(caller: func) ~(caller_input: 't)
        ~(caller_callsites: 't callsite list)
        ~(callee: func) ~(callee_input: 't) (instr_call: instr) : 't callsite =
    { cs_instr_call = instr_call;
      cs_caller = caller;
      cs_caller_input = caller_input;
      cs_caller_callsites = caller_callsites;
      cs_callee = callee;
      cs_callee_input = callee_input; }

  method mk_exception expr (data: 't) tinfo =
    let tinfo = get_root_src_of_bitcast tinfo in
    { exn_orig_expr = expr;
      exn_root_expr = mk_expr_exn tinfo;
      exn_data = data;
      exn_type_info = tinfo; }

  method mk_working_block blk instr (input: 't) =
    { wb_block = blk;
      wb_instr = instr;
      wb_instr_input = input; }

  method mk_global_env () =
    { genv_global_output = Hashtbl.create (module Global);
      genv_globals_data = least_data; }

  method mk_func_env (prog: program) func (input: 't) ~(callsites: 't callsite list)
           : 't func_env =
    { fenv_id = "";
      fenv_func = func;
      fenv_callsites = callsites;
      fenv_prog = prog;
      fenv_instr_output = Hashtbl.create (module Instr);
      fenv_block_input = Hashtbl.create (module Block);
      fenv_input = input;
      fenv_output = None;
      fenv_thrown_exn = Hashtbl.create (module Llvalue);
      fenv_landing_exns = Hashtbl.create (module Llvalue);
      fenv_deref_params = [];
      fenv_deref_globals = [];
      fenv_working_blocks = [];
      fenv_state_changed = false; }

  method mk_prog_env (prog: program) : 't prog_env =
    { penv_prog = prog;
      penv_global_env = self#mk_global_env ();
      penv_func_envs = Hashtbl.create (module Func);
      penv_func_summaries = Hashtbl.create (module Func);
      penv_sparse_llvalue = Hashtbl.create (module Llvalue);
      penv_sparse_block = Hashtbl.create (module Block);
      penv_sparse_func = Hashtbl.create (module Func);
      penv_sparse_used_globals = Hashtbl.create (module Func);
      penv_block_sparse_instrs = Hashtbl.create (module Block);
      penv_sparse_precedings_block = Hashtbl.create (module Block);
      penv_sparse_succeedings_block = Hashtbl.create (module Block);
      penv_sparse_reachable_blocks = Hashtbl.create (module Block);
      penv_func_analyzed_times = Hashtbl.create (module Func);
      penv_block_local_analyzed_times = Hashtbl.create (module Block);
      penv_block_total_analyzed_times = Hashtbl.create (module Block);
      penv_working_funcs = [];
      penv_goal_funcs = [];
      penv_func_analyzed_inputs = Hashtbl.create (module Func);
      penv_func_analysis_stack = Stack.create ();
      penv_block_analyzed_squence = Hashtbl.create (module Func); }

  method copy_func_env (fenv : 't func_env)  : 't func_env =
    { fenv_id = fenv.fenv_id;
      fenv_func = fenv.fenv_func;
      fenv_callsites = fenv.fenv_callsites;
      fenv_prog = fenv.fenv_prog;
      fenv_instr_output = Hashtbl.copy fenv.fenv_instr_output;
      fenv_block_input = Hashtbl.copy fenv.fenv_block_input;
      fenv_input = fenv.fenv_input;
      fenv_output = fenv.fenv_output;
      fenv_thrown_exn = Hashtbl.copy fenv.fenv_thrown_exn;
      fenv_landing_exns = Hashtbl.copy fenv.fenv_landing_exns;
      fenv_deref_params = fenv.fenv_deref_params;
      fenv_deref_globals = fenv.fenv_deref_globals;
      fenv_working_blocks = fenv.fenv_working_blocks;
      fenv_state_changed = fenv.fenv_state_changed; }

  (* comparison *)

  method equal_callsite (cs1: 't callsite) (cs2: 't callsite) =
    equal_func cs1.cs_caller cs2.cs_caller &&
    (* T.equal_data cs1.cs_caller_input cs2.cs_caller_input && *)
    equal_instr cs1.cs_instr_call cs2.cs_instr_call &&
    equal_func cs1.cs_callee cs2.cs_callee
  (* T.equal_data cs1.cs_callee_input cs2.cs_callee_input *)

  method equal_callsites (css1: 't callsites) (css2: 't callsites) =
    List.length css1 = List.length css2 &&
    List.for_all ~f:(List.mem ~equal:self#equal_callsite css2) css1

  method equal_working_block (wb1: 't working_block) (wb2: 't working_block) : bool =
    equal_block wb1.wb_block wb2.wb_block &&
    equal_instr wb1.wb_instr wb2.wb_instr

  method equal_working_func (fc1: 't working_func) (fc2: 't working_func) : bool =
    equal_func fc1.wf_func fc2.wf_func &&
    self#equal_data fc1.wf_input fc2.wf_input

  (* getter and setter *)

  method get_func_summaries (penv: 't prog_env) func : 't func_summary list =
    match Hashtbl.find penv.penv_func_summaries func with
    | Some fsums -> fsums
    | None -> []

  method get_suitable_func_summary penv func input : 't func_summary option =
    let _ = hdebug ~always:true ~indent:4 "Get suitable func summary: " func_name func in
    let _ = hdebug ~always:true ~indent:4 "  Input: " self#pr_data input in
    let fsums = self#get_func_summaries penv func in
    let res = List.find ~f:(fun fs ->
        let _ = hdebug ~indent:4 "  Fsum input: " self#pr_data fs.fsum_input in
        let res = self#equal_data fs.fsum_input input in
        let _ = hdebug ~indent:4 "    equal? " pr_bool res in
        res) fsums in
    (* let res = List.find ~f:(fun fs ->
     *   let _ = hdebug ~indent:4 "  Fsum input: " pr_data fs.fsum_input in
     *   let res = T.lequal_data input fs.fsum_input in
     *   let _ = hdebug ~indent:4 "    lequal? " pr_bool res in
     *   res) fsums in *)
    let _ = match res with
      | None -> hdebug ~always:true ~indent:4 "Func summary NOT FOUND: " func_name func
      | Some fsum -> hdebug ~always:true ~indent:4 "Found func summary:\n" self#pr_func_summary fsum in
    res

  method record_func_summary (penv: 't prog_env) fsum : unit =
    let func = fsum.fsum_func in
    let _ = hdebug "Record current function summary: " func_name func in
    let _ = hdebug "  input: " self#pr_data fsum.fsum_input in
    let _ = debug ~always:true ("Record current func summary: " ^ (func_name func) ^
                                " @ {" ^ (self#pr_data fsum.fsum_input) ^ "}") in
    match Hashtbl.find penv.penv_func_summaries func with
    | None ->
        Hashtbl.set penv.penv_func_summaries ~key:func ~data:[fsum]
    | Some fsums ->
        let fsums, updated =  List.fold ~f:(fun (accf, accu) fs ->
            if self#equal_data fs.fsum_input fsum.fsum_input then
              (accf @ [fsum], true)
            else (accf @ [fs], accu)) ~init:([], false) fsums in
        let nfsums = if updated then fsums else fsums @ [fsum] in
        Hashtbl.set penv.penv_func_summaries ~key:func ~data:nfsums

  method find_func_env_by_input penv (f: func) (input: 't) : 't func_env option =
    match Hashtbl.find penv.penv_func_envs f with
    | None -> None
    | Some fenvs ->
        List.find ~f:(fun fenv -> self#equal_data fenv.fenv_input input) fenvs

  (* let remove_fenv_of_callsites penv (f: func) callsites : unit =
   *   match Hashtbl.find penv.penv_func_envs f with
   *   | None -> None
   *   | Some fenvs ->
   *     let nfenvs = List.fold_left ~f:(fun acc fenv ->
   *       fenv.
   *     ) ~init:[] fenvs
   *     List.find ~f:(fun fenv -> T.equal_data fenv.fenv_input input) fenvs *)


  method is_fenv_updated (oldfe: 't func_env) (newfe: 't func_env) : bool =
    match oldfe.fenv_output, newfe.fenv_output with
    | Some d1, Some d2 -> not (self#equal_data d1 d2)
    | None, Some _ -> true
    | Some _, None -> true
    | None, None -> newfe.fenv_state_changed

  method record_new_func_env (penv: 't prog_env) fenv : bool * bool * bool * bool =
    let func = fenv.fenv_func in
    let _ = hdebug ~always:true "Record new function env: " func_name func in
    let _ = hdebug ~always:true " - Callsites: " self#pr_callsites fenv.fenv_callsites in
    let _ = hdebug " - Output: " self#pr_data_opt fenv.fenv_output in
    let env_completed =
      try
        let fblock = Some (fun blk ->
            if self#is_sparse_block penv blk then None
            else Some ()) in
        let finstr = Some (fun instr ->
            if self#is_sparse_instr penv instr && not (is_instr_unreachable instr) then
              match self#get_instr_output fenv instr with
              | None ->
                  (* let _ = hdebug ~always:true "Env incompleted at: " pr_instr instr in *)
                  let _ = hdebug "Env incompleted at: " pr_instr instr in
                  raise (EBool false)
              | Some _ -> ()) in
        let _ = deep_iter_func ~fblock ~finstr func in
        true
      with EBool res -> res in
    let _ = hdebug ~always:true " - Env completed: " pr_bool env_completed in
    match Hashtbl.find penv.penv_func_envs func with
    | None ->
        let fenv = {fenv with fenv_id = "1"} in
        let _ = Hashtbl.set penv.penv_func_envs ~key:func ~data:[fenv] in
        (* let _ = debug "Initial fenv" in *)
        (true, true, true, env_completed)
    | Some fenvs ->
        let fenvs, input_updated, output_updated, env_updated =
          List.fold ~f:(fun (accf, acci, acco, acce) fe ->
              if self#equal_data fe.fenv_input fenv.fenv_input then
                let fenv = {fenv with fenv_id = fe.fenv_id} in
                let naccf = accf @ [fenv] in
                let nacce = self#is_fenv_updated fe fenv in
                let nacco = match fe.fenv_output, fenv.fenv_output with
                  | None, None -> false
                  | None, Some _ | Some _, None -> true
                  | Some d1, Some d2 -> not (self#equal_data d1 d2) in
                (naccf, false, nacco, nacce)
              else (accf @ [fe], acci, acco, acce)) ~init:([], true, true, true) fenvs in
        let nfenvs =
          if input_updated then
            let fenv = {fenv with fenv_id = pr_int ((List.length fenvs) + 1)} in
            fenvs @ [fenv]
          else fenvs in
        let _ = Hashtbl.set penv.penv_func_envs ~key:func ~data:nfenvs in
        let _ = hdebug ~always:true " - Input updated: " pr_bool input_updated in
        let _ = hdebug ~always:true " - Output updated: " pr_bool output_updated in
        let _ = hdebug ~always:true " - Env updated: " pr_bool env_updated in
        (input_updated, output_updated, env_updated, env_completed)

  method is_func_input_analyzed (penv: 't prog_env) func (input: 't) : bool =
    match Hashtbl.find penv.penv_func_analyzed_inputs func with
    | Some inputs -> List.exists ~f:(self#equal_data input) inputs
    | None -> false

  method get_func_analyzed_inputs (penv: 't prog_env) func : 't list =
    match Hashtbl.find penv.penv_func_analyzed_inputs func with
    | Some inputs -> inputs
    | None -> []

  method record_func_analyzed_input (penv: 't prog_env) fenv (input: 't) : unit =
    let func = fenv.fenv_func in
    let _ = hdebug ~always:true "Record analyzed input of func: " func_name func in
    let _ = hdebug ~always:true " - Callsites: " self#pr_callsites fenv.fenv_callsites in
    let _ = hdebug " - Input: " self#pr_data input in
    let _ = print ("Record analyzed input: " ^ (func_name func) ^ " @ {" ^
                   (self#pr_data input) ^ "}") in
    match Hashtbl.find penv.penv_func_analyzed_inputs func with
    | Some inputs ->
        let inputs = List.insert_dedup ~equal:self#equal_data inputs input in
        Hashtbl.set penv.penv_func_analyzed_inputs ~key:func ~data:inputs
    | None ->
        Hashtbl.set penv.penv_func_analyzed_inputs ~key:func ~data:[input]

  method reset_analysis_env (penv: 't prog_env) (fenv: 't func_env) =
    let prog, func = penv.penv_prog, fenv.fenv_func in
    let _ = fenv.fenv_state_changed <- false in
    let _ = iter_blocks ~f:(fun blk ->
        Hashtbl.remove penv.penv_block_local_analyzed_times blk) func in
    ()
  (* if List.not_empty fenv.fenv_working_blocks then (
   *   let init_blks = List.map ~f:(fun wb -> wb.wb_block) fenv.fenv_working_blocks in
   *   let blks = List.fold_left ~f:(fun acc wb ->
   *     let blk = wb.wb_block in
   *     List.concat_dedup ~equal:equal_block acc (get_reachable_blocks prog blk)
   *   ) ~init:fenv.fenv_working_blocks fenv.fenv_working_blocks in
   *   let _ = List.iter ~f:(fun blk ->
   *     iter_instrs ~f:(fun instr ->
   *       Hashtbl.remove fenv.fenv_instr_output instr) blk) blks in
   *   fenv.fenv_output <- None) *)

  (* utilities *)

  method is_intra_proc_dfa_mode () =
    !dfa_mode == DfaIntraProc

  method merge_datas (datas: 't list) : 't =
    match datas with
    | [] -> least_data
    | d::nds -> List.fold_left ~f:(self#merge_data ~widen:false) ~init:d nds

  method mk_working_func (f: func) (input: 't) (cs: 't callsites) : 't working_func =
    { wf_callsites = cs;
      wf_func = f;
      wf_input = input; }

  method mk_func_summary (penv: 't prog_env) (fenv: 't func_env) input : 't func_summary =
    let prog, func = fenv.fenv_prog, fenv.fenv_func in
    let _ = hdebug "fenv_output: " self#pr_data_opt fenv.fenv_output in
    let output = match fenv.fenv_output with
      | None -> herror "mk_func_summary: output not found: " func_name func
      | Some data ->  self#clean_irrelevant_info_from_data penv func data in
    { fsum_func = func;
      fsum_input = input;
      fsum_output = output;
      fsum_thrown_exn = Hashtbl.data fenv.fenv_thrown_exn;
      fsum_deref_params = fenv.fenv_deref_params;
      fsum_deref_globals = fenv.fenv_deref_globals; }

  (* TODO: need to include existing input to,
     similarly to the frame rule *)
  method compute_call_output_exns_from_summary (penv: 't prog_env) instr callee
           args input fsum : 't * ('t exns) =
    self#compute_callee_output_exns penv instr callee args input fsum

  (* working with pending analyzing function *)

  method compute_core_callee_input (penv: 't prog_env) instr callee args (input: 't) =
    let prog = penv.penv_prog in
    let res = self#prepare_callee_input penv instr callee args input in
    res

  method equal_args_data_vs_analyzed_inputs (penv: 't prog_env) callee args core_input : bool =
    let fname = func_name callee in
    let analyzed_inputs = self#get_func_analyzed_inputs penv callee in
    let res = List.exists ~f:(self#equal_data core_input) analyzed_inputs in
    (* let res = List.exists ~f:(T.lequal_data core_input) analyzed_inputs in *)
    let _ = debug ~indent:4 ("Compare args' input data and summary of: " ^ fname) in
    let _ = hdebug " - input: " self#pr_data core_input in
    let _ = hdebug " - analyzed inputs: " (self#pr_datas ~bullet:"  +") analyzed_inputs in
    let _ = hdebug " - res (<=): " pr_bool res in
    res

  method record_working_func ?(msg="") (penv: 't prog_env) (wf: 't working_func) : unit =
    let new_wf = ref true in
    let wfuncs = List.map ~f:(fun wf2 ->
        if equal_func wf.wf_func wf2.wf_func then (
          if self#equal_data wf.wf_input wf2.wf_input then
            let _ = new_wf := false in
            let callsites = List.concat_dedup wf.wf_callsites wf2.wf_callsites
                              ~equal:self#equal_callsite in
            let nwf = {wf2 with wf_callsites = callsites} in
            (* let _ = hprint "Update callsistes of working function: " pr_working_func nwf in *)
            nwf
          else if List.for_all ~f:(fun cs1 ->
              List.exists ~f:(fun cs2 ->
                  equal_instr cs1.cs_instr_call cs2.cs_instr_call) wf2.wf_callsites) wf.wf_callsites then
            (* let callsites = List.concat_dedup wf.wf_callsites wf2.wf_callsites
             *                   ~equal:equal_callsite in *)
            let _ = new_wf := false in
            (* let _ = print ("Replace working function:\n" ^
             *                "     - " ^ (pr_working_func wf2) ^ "\n" ^
             *                " by: - " ^ (pr_working_func wf)) in *)
            (* let nwf = {wf with wf_callsites = callsites} in *)
            let nwf = wf in
            nwf
          else wf2)
        else wf2) penv.penv_working_funcs in
    let nwfuncs =
      if !new_wf then
        let fname, input = func_name wf.wf_func, wf.wf_input in
        let fname =
          if String.is_empty msg then fname
          else fname ^ " (" ^ msg ^ ")" in
        let _ = hdebug ("-> ENQUEUE function: " ^ fname ^ ", with input: ") self#pr_data input in
        wfuncs @ [wf]
      else wfuncs in
    let _ = penv.penv_working_funcs <- nwfuncs in
    ()


  method enqueue_to_analyze_func ?(msg="") (penv: 't prog_env) (wf: 't working_func) : unit =
    (* update input *)
    let func, input, callsites = wf.wf_func, wf.wf_input, wf.wf_callsites in
    let fname = func_name func in
    let res, time = track_runtime (fun () ->
        match callsites with
        | [] ->
            let wf = self#mk_working_func func input callsites in
            self#record_working_func ~msg penv wf
        | _ ->
            let _ = hdebug ~always:true "   at call site: " self#pr_callsites callsites in
            let wfuncs = penv.penv_working_funcs in
            let has_earlier_working_func_of_same_context = List.exists ~f:(fun wf ->
                equal_func func wf.wf_func &&
                List.for_all ~f:(fun cs1 ->
                    List.exists ~f:(fun cs2 ->
                        equal_instr cs1.cs_instr_call cs2.cs_instr_call &&
                        self#equal_data cs1.cs_callee_input cs2.cs_callee_input
                      ) wf.wf_callsites
                  ) callsites) wfuncs in
            (* let has_earlier_working_func_of_similar_context =
             *   List.exists ~f:(fun wf ->
             *     equal_func func wf.wf_func &&
             *     List.for_all ~f:(fun cs1 ->
             *       List.exists ~f:(fun cs2 ->
             *         equal_instr cs1.cs_instr_call cs2.cs_instr_call &&
             *         equal_data cs1.cs_callee_input cs2.cs_callee_input
             *       ) wf.wf_callsites
             *     ) callsites) wfuncs in *)
            if not (self#is_func_input_analyzed penv func input) &&
               not has_earlier_working_func_of_same_context then (
              self#record_working_func ~msg penv wf)
            else debug ~always:true ("-> SKIP enqueuing: " ^ fname)) in
    (* let _ = hprint (" - Time enqueue function: " ^ fname ^ ": ")
     *           (sprintf "%.3fs") time in *)
    res

  (* working with analyzed function *)

  method get_func_analyzed_times (penv: 't prog_env) func : int =
    match Hashtbl.find penv.penv_func_analyzed_times func with
    | Some n -> n
    | None -> 0

  method update_func_analyzed_stats (penv: 't prog_env) func : unit =
    let _ = Stack.push penv.penv_func_analysis_stack func in
    let n = match Hashtbl.find penv.penv_func_analyzed_times func with
      | Some n -> n
      | None -> 0 in
    Hashtbl.set penv.penv_func_analyzed_times ~key:func ~data:(n+1)

  method get_block_session_analyzed_times (penv: 't prog_env) blk : int =
    match Hashtbl.find penv.penv_block_local_analyzed_times blk with
    | Some n -> n
    | None -> 0

  method get_block_analyzed_sequence (penv: 't prog_env) func : blocks =
    match Hashtbl.find penv.penv_block_analyzed_squence func with
    | None -> []
    | Some blks -> blks

  method update_block_analyzed_stats (penv: 't prog_env) func blk : unit =
    let _ =
      let blks = (self#get_block_analyzed_sequence penv func) @ [blk] in
      Hashtbl.set penv.penv_block_analyzed_squence ~key:func ~data:blks in
    let _ =
      let n = match Hashtbl.find penv.penv_block_local_analyzed_times blk with
        | Some n -> n
        | None -> 0 in
      Hashtbl.set penv.penv_block_local_analyzed_times ~key:blk ~data:(n+1) in
    let _ =
      let n = match Hashtbl.find penv.penv_block_total_analyzed_times blk with
        | Some n -> n
        | None -> 0 in
      Hashtbl.set penv.penv_block_total_analyzed_times ~key:blk ~data:(n+1) in
    ()

  method is_call_to_user_function (instr: instr) : bool =
    match instr_opcode instr with
    | LO.Call -> is_user_func (callee_of_instr_call instr)
    | _ -> false

  method is_call_to_func_pointer (instr: instr) : bool =
    match instr_opcode instr with
    | LO.Call -> is_func_pointer (callee_of_instr_call instr)
    | _ -> false

  (** compute input of block *)
  method compute_block_input penv ?(widen=false) (fenv: 't func_env) blk : 't =
    let _ = hdebug "Compute input of block: " block_name blk in
    let prog = fenv.fenv_prog in
    (* let _ = hdebugc "  pathcond: " pr_pathcond pcond in *)
    (* let pblks = get_preceding_blocks prog blk |>
     *             List.map ~f:(fun pb -> pb.pblk_block) in *)
    let pblks = self#get_sparse_preceding_blocks penv blk in
    (* let _ = hdebug ("Sparse preceding blocks of " ^ (block_name blk) ^ ": ")
     *           block_names pblks in *)
    let pblks_data = List.fold_left ~f:(fun acc pblk ->
        let pbname = block_name pblk in
        match last_instr_of_block pblk with
        | None -> acc
        | Some instr ->
            let output = self#get_instr_output fenv instr in
            match output with
            | None -> acc
            | Some output ->
                let pred =
                  if !dfa_sparse_analysis then None
                  else match get_branch instr with
                    | None -> None
                    | Some (`Unconditional _) -> None
                    | Some (`Conditional (instr_cond, blk1, blk2)) ->
                        let pred = extract_br_cond_predicate instr_cond in
                        if equal_block_name blk blk1 then Some pred
                        else Some (mk_pred_neg pred) in
                let output = match pred with
                  | None -> output
                  | Some pred ->
                      let _ = hdebug "Preceding block's current output: " self#pr_data output in
                      let new_output =
                        match self#get_block_input fenv blk with
                        | None -> self#refine_data_by_predicate ~widen output pred
                        | Some old_output ->
                            if self#is_data_satisfied_predicate output pred then output
                            else old_output in
                      let _ = hdebug "Preceding block's refined output: " self#pr_data new_output in
                      new_output in
                (* let changed = not (self#lequal_data new_output output) in *)
                acc @ [output]) ~init:[] pblks in
    let new_block_input = match pblks_data with
      | [] -> fenv.fenv_input   (* entry block *)
      | [d] -> d
      | d::ds -> List.fold_left ~f:(self#merge_data ~widen:false) ~init:d ds in
    new_block_input

  method record_func_output (fenv: 't func_env) (output: 't) : unit =
    match fenv.fenv_output with
    | None -> fenv.fenv_output <- Some output
    | Some old_output ->
        let new_ouput = self#merge_data old_output output in
        fenv.fenv_output <- Some new_ouput

  (** analyze function call to a user function *)
  method analyze_instr_call_user_func (penv: 't prog_env) (fenv: 't func_env)
           instr callee args input : 't * bool =
    let prog = penv.penv_prog in
    let caller = func_of_instr instr in
    let caller_input = fenv.fenv_input in
    if self#is_intra_proc_dfa_mode () then (input, true)
    else if not (self#is_sparse_func penv callee) then
      (input, true)
    else
      let refined_argss =
        if !dfa_context_split_phi then
          let _ = hdebug "Do context splitting PHI for the call: "
                    pr_instr instr in
          let phi_incoming_blks = List.fold_left ~f:(fun acc arg ->
              if is_llvalue_instr_phi arg then
                let blks = arg |> LL.incoming |> List.unzip |> snd in
                List.concat_dedup acc blks ~equal:equal_block
              else acc) ~init:[] args in
          if List.is_empty phi_incoming_blks then [args]
          else List.map ~f:(fun blk ->
              List.map ~f:(fun arg ->
                  if is_llvalue_instr_phi arg then
                    let incoming = LL.incoming arg in
                    let src_arg_blk = List.find ~f:(fun (v, vblk) ->
                        equal_block blk vblk) incoming in
                    match src_arg_blk with
                    | None -> arg
                    | Some (v, _) -> v
                  else arg) args
            ) phi_incoming_blks
        else [args] in
      let origin_args = args in
      let outputs_continues = refined_argss |> List.map ~f:(fun args ->
          let _ = hdebug ~always:true "Calling to: " func_name callee in
          let _ = hdebug "   with args: " pr_values origin_args in
          let _ = hdebug "   refined args: " pr_values args in
          let callee_input =
            if !dfa_context_split_phi then
              let phi_args_of_call_only = List.filter ~f:(fun oarg ->
                  if is_llvalue_instr_phi oarg then
                    let users = get_users oarg in
                    List.for_all ~f:is_llvalue_callable_instr users
                  else false) origin_args in
              let _ = self#clean_info_of_vars input phi_args_of_call_only in
              self#compute_core_callee_input penv instr callee args input
            else self#compute_core_callee_input penv instr callee args input in
          let _ =
            if self#get_func_analyzed_times penv callee == 0 ||
               not (self#equal_args_data_vs_analyzed_inputs penv callee args callee_input) then (
              let caller_callsites = fenv.fenv_callsites in
              let callsites = [self#mk_callsite instr ~caller ~caller_input
                                 ~caller_callsites ~callee ~callee_input] in
              (* enqueue to analyze callee *)
              let wf_callee = self#mk_working_func callee callee_input callsites in
              let _ = hdebug ~always:true "Prepare to enqueue callee: " func_name
                        wf_callee.wf_func in
              let _ = self#enqueue_to_analyze_func ~msg:"callee" penv wf_callee in
              (* enqueue to re-analyze the current block *)
              let blk = block_of_instr instr in
              let wb = self#mk_working_block blk instr input in
              let _ = hdebug ~indent:4 "Mark working block to re-analyze: "
                        self#pr_working_block wb in
              let working_blks = List.insert_dedup fenv.fenv_working_blocks wb
                                   ~equal:self#equal_working_block in
              fenv.fenv_working_blocks <- working_blks)
            else hdebug "Not enqueue: " func_name callee in
          let _ = hdebug "callee input: " self#pr_data callee_input in
          match self#get_suitable_func_summary penv callee callee_input with
          | None -> (least_data, false)
          (* FIXME: check if need to use the current data or the least data? *)
          | Some fsum ->
              (* compute output, exceptions *)
              (* let _ = hprint "Found a function summary of: " func_name callee in *)
              let output, exns = self#compute_call_output_exns_from_summary penv instr callee
                                   args input fsum in
              (* update exception *)
              let _ = List.iter ~f:(fun exn ->
                  let tinfo = exn.exn_type_info in
                  match Hashtbl.find fenv.fenv_thrown_exn tinfo with
                  | None -> Hashtbl.set fenv.fenv_thrown_exn ~key:tinfo ~data:exn
                  | Some cur_exn ->
                      let ndata = self#merge_data exn.exn_data cur_exn.exn_data in
                      let nexn = {exn with exn_data = ndata} in
                      Hashtbl.set fenv.fenv_thrown_exn ~key:tinfo ~data:nexn
                ) exns in
              output, true) in
      match outputs_continues with
      | [] -> (least_data, false)
      | (output, continue)::noutputs_continues ->
          let noutputs, ncontinues = List.unzip noutputs_continues in
          let noutput = List.fold_left ~f:(self#merge_data ~widen:true) ~init:output noutputs in
          let ncontinue = List.fold_left ~f:(||) ~init:continue ncontinues in
          (noutput, ncontinue)

  method analyze_instr_throw_exception (penv: 't prog_env) fenv instr callee args input :
           unit =
    if List.length args >= 2 then (
      let ptr = List.nth_exn args 0 in
      let tinfo = get_root_src_of_bitcast (List.nth_exn args 1) in
      let exn_data = self#prepare_thrown_exception_data penv ptr tinfo input in
      let exn = self#mk_exception (mk_expr_var ptr) exn_data tinfo in
      match Hashtbl.find fenv.fenv_thrown_exn tinfo with
      | None -> Hashtbl.set fenv.fenv_thrown_exn ~key:tinfo ~data:exn
      | Some cur_exn ->
          let ndata = self#merge_data exn.exn_data cur_exn.exn_data in
          let nexn = {exn with exn_data = ndata} in
          Hashtbl.set fenv.fenv_thrown_exn ~key:tinfo ~data:nexn)
    else herror "analyze_throw_exception: expect >=2 params: " pr_instr instr

  method analyze_instr_landingpad (penv: 't prog_env) (fenv: 't func_env) instr : unit =
    match instr_opcode instr with
    | LO.LandingPad ->
        let num_catches = num_operands instr in
        let landing_exns = ref [] in
        let _ = for i = 0 to num_catches - 1 do
            let exn_tinfo = get_root_src_of_bitcast (operand instr i) in
            let _ = hdebug "landingpad: " pr_value exn_tinfo in
            match Hashtbl.find fenv.fenv_thrown_exn exn_tinfo with
            | None -> ()
            | Some exn ->
                (* first remove the exceptions from fenv *)
                let _ = Hashtbl.remove fenv.fenv_thrown_exn exn_tinfo in
                landing_exns := !landing_exns @ [exn]
          done in
        let landing_ptr = llvalue_of_instr instr in
        let _ = hdebug "landing exns: " self#pr_exns !landing_exns in
        Hashtbl.set fenv.fenv_landing_exns ~key:landing_ptr ~data:!landing_exns
    | _ -> herror "analyze_instr_landingpad: not a landingpad: " pr_instr instr

  method get_catch_exception_type_info (penv: 't prog_env) (fenv: 't func_env) instr : llvalue option =
    let catch_blk = block_of_instr instr in
    let pblks = get_preceding_blocks penv.penv_prog catch_blk in
    if List.length pblks = 1 then
      let pblk = List.hd_exn pblks in
      match pblk.pblk_pathcond with
      | PIcmp (LL.Icmp.Eq, v1, v2) when is_llvalue_instr v1 && is_llvalue_instr v2 ->
          let iv1, iv2 = mk_instr v1, mk_instr v2 in
          if is_instr_call_invoke iv1 && is_instr_extractvalue iv2 &&
             is_func_eh_typeid_for (callee_of_instr_func_call iv1) then
            let args = args_of_instr_func_app iv1 in
            Some (get_root_src_of_bitcast (List.hd_exn args))
          else if is_instr_call_invoke iv2 && is_instr_extractvalue iv1 &&
                  is_func_eh_typeid_for (callee_of_instr_func_call iv2) then
            let args = args_of_instr_func_app iv2 in
            Some (get_root_src_of_bitcast (List.hd_exn args))
          else None
      | _ -> None
    else None

  method analyze_instr_catch_exception (penv: 't prog_env) (fenv: 't func_env) instr args input : 't * bool =
    if List.length args = 1 then
      let catched_ptr = List.hd_exn args in
      match LL.classify_value catched_ptr with
      | LV.Instruction LO.ExtractValue ->
          let landing_ptr = LL.operand catched_ptr 0 in
          let exn = match Hashtbl.find fenv.fenv_landing_exns landing_ptr with
            | None -> None
            | Some exns -> match self#get_catch_exception_type_info penv fenv instr with
              | None -> None
              | Some tinfo ->
                  let exns = List.filter ~f:(fun e ->
                      equal_llvalue tinfo e.exn_type_info) exns in
                  Some (List.hd_exn exns) in
          let res = match exn with
            | None -> (input, true)
            | Some exn ->
                let output = self#compute_catch_exception_data penv instr catched_ptr input exn in
                let _ = hdebug "output after catch exn: " self#pr_data output in
                (output, true) in
          res
      | _ -> (input, true)
    else (input, true)

  method analyze_instrs ?(widen=false) (penv: 't prog_env) (fenv: 't func_env) instrs input : bool * bool =
    match instrs with
    | [] -> (true, true)
    | instr::ninstrs ->
        let _ = hdebug ">> " pr_instr instr in
        let _ = hdebug "    In:  " self#pr_data input in
        let old_output = self#get_instr_output fenv instr in
        let new_output, continue = match instr_opcode instr with
          | LO.LandingPad ->
              let _ = self#analyze_instr_landingpad penv fenv instr in
              (input, true)
          | LO.Call | LO.Invoke ->
              let callee = callee_of_instr_func_call instr in
              let args = args_of_instr_func_app instr in
              if is_func_clang_call_terminate callee then
                (input, true)
              else if is_func_throw_exception callee then
                let _ = self#analyze_instr_throw_exception penv fenv instr callee args input in
                (input, true)
              else if is_func_begin_catch_exception callee then
                self#analyze_instr_catch_exception penv fenv instr args input
              else if is_user_func callee then (
                let res, time = track_runtime (fun () ->
                    self#analyze_instr_call_user_func penv fenv instr callee args input) in
                res)
              else if is_func_pointer callee then (
                let _ = if is_pointer_analysis analysis then (
                    (* TODO: restructure this part *)
                    let _ = self#analyze_instr ~widen penv fenv instr input in
                    ())in
                let ptr = llvalue_of_func callee in
                let callees = ptr |> get_current_funcs_of_pointer penv.penv_prog in
                let _ = hdebug ~always:true "Function pointer: " pr_value ptr in
                let _ = hdebug ~always:true "Callees: " func_names callees in
                if List.is_empty callees then
                  (* (input, false) *)
                  (input, true)
                else if List.exists ~f:is_library_function callees then
                  (input, true)
                else
                  let res, time = track_runtime (fun () ->
                      let outputs_continues = List.map ~f:(fun f ->
                          self#analyze_instr_call_user_func penv fenv instr f args input) callees in
                      let outputs, continues = List.unzip outputs_continues in
                      self#merge_datas outputs, List.fold_left ~f:(||) ~init:false continues) in
                  (* let _ = hprint ("  Time analyzing instr_func_call:\n   " ^
                   *                 (pr_instr instr) ^ ":\n     ")
                   *           (sprintf "%.3fs") time in *)
                  res)
              else (self#analyze_instr ~widen penv fenv instr input, true)
          | LO.Ret ->
              let output = self#analyze_instr ~widen penv fenv instr input in
              let _ = self#record_func_output fenv output in
              (output, true)
          | LO.Unreachable ->
              let output = least_data in
              (* (output, true) *)
              (output, false)
          | _ -> (self#analyze_instr ~widen penv fenv instr input, true) in
        (* let _ = hdebugc " Old out:" pr_data_opt old_output in *)
        let changed = match old_output with
          | None -> true
          | Some old_output -> not (self#lequal_data new_output old_output) in
        let _ = if continue then self#set_instr_output fenv instr new_output in
        let _ = hdebug "    Out: " self#pr_data new_output in
        let _ = ndebug ("    Changed: " ^ (pr_bool changed) ^ "\n" ^
                        "    Continue: " ^ (pr_bool continue)) in
        if not continue then (changed, false)
        else if not changed then (false, continue)
        else self#analyze_instrs penv fenv ninstrs new_output


  (** analyze one basic block *)
  method analyze_block ?(widen=false) (penv: 't prog_env) (fenv: 't func_env) (wb: 't working_block) : bool * bool =
    let prog, func, blk = penv.penv_prog, fenv.fenv_func, wb.wb_block in
    let _ =
      let n = self#get_block_session_analyzed_times penv blk in
      print (sprintf "=> Analyzing block: %s ~ (%d)" (block_name blk) n) in
    let _ =
      if is_first_instr_of_block wb.wb_instr then
        let binput = wb.wb_instr_input in
        let _ = self#set_block_input fenv blk binput in
        let _ = debug " - Starting from the entry of the block. " in
        hdebug "    Block input: " self#pr_data binput
      else
        hdebug " - Continuing from instruction: " pr_instr wb.wb_instr in
    (* let _ = hprint "  Time compute block input: " (sprintf "%.3fs") time in *)
    (* let changed = ref input_changed in *)
    let changed = ref false in
    let _ = self#update_block_analyzed_stats penv func blk in
    let instrs, _ = fold_left_instrs ~f:(fun (acci, accs) instr ->
        if equal_instr instr wb.wb_instr || accs then
          if self#is_sparse_instr penv instr then (acci @ [instr], true)
          else (acci, true)
        else (acci, accs)) ~init:([], false) blk in
    self#analyze_instrs ~widen penv fenv instrs wb.wb_instr_input

  method compare_block_by_sparse_reachability (penv: 't prog_env)
           (wblk1: 't working_block) (wblk2: 't working_block) : int =
    let blk1, blk2 = wblk1.wb_block, wblk2.wb_block in
    let reachable12 = self#is_sparse_reachable_block penv blk1 blk2 in
    let reachable21 = self#is_sparse_reachable_block penv blk2 blk1 in
    if reachable12 && not reachable21 then 1
    else if not reachable12 && reachable21 then -1
    else 0

  method compare_block_by_name (penv: 't prog_env)
           (wblk1: 't working_block) (wblk2: 't working_block) : int =
    let blk1, blk2 = wblk1.wb_block, wblk2.wb_block in
    compare_block_by_name blk1 blk2

  method compare_block_by_distance_from_entry (penv: 't prog_env)
           (wblk1: 't working_block) (wblk2: 't working_block) =
    let blk1, blk2 = wblk1.wb_block, wblk2.wb_block in
    let prog, func = penv.penv_prog, func_of_block blk1 in
    let bg = LG.get_block_graph prog func in
    let entry = entry_block func in
    let distance1 = LG.shortest_block_distance bg entry blk1 in
    let distance2 = LG.shortest_block_distance bg entry blk2 in
    match distance1, distance2 with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some n1, Some n2 ->
        if n1 < n2 then -1
        else if n1 > n2 then 1
        else 0

  method compare_block_by_num_pair_pred_succ_blocks (penv: 't prog_env)
           (wblk1: 't working_block) (wblk2: 't working_block) =
    let prog = penv.penv_prog in
    let blk1, blk2 = wblk1.wb_block, wblk2.wb_block in
    let num_pred1 = self#get_sparse_preceding_blocks penv blk1 |> List.length in
    let num_pred2 = self#get_sparse_preceding_blocks penv blk2 |> List.length in
    let num_succ1 = self#get_sparse_succeeding_blocks penv blk1 |> List.length in
    let num_succ2 = self#get_sparse_succeeding_blocks penv blk2 |> List.length in
    let num_pair_pred_succ1 = num_pred1 * num_succ1 in
    let num_pair_pred_succ2 = num_pred2 * num_succ2 in
    if num_pair_pred_succ1 < num_pair_pred_succ2 then -1
    else if num_pair_pred_succ1 > num_pair_pred_succ2 then 1
    else 0

  method compare_block_by_analyzed_times (penv: 't prog_env)
           (wblk1: 't working_block) (wblk2: 't working_block) : int =
    let blk1, blk2 = wblk1.wb_block, wblk2.wb_block in
    let num_analyzed1 = self#get_block_session_analyzed_times penv blk1 in
    let num_analyzed2 = self#get_block_session_analyzed_times penv blk2 in
    if num_analyzed1 < num_analyzed2 then -1
    else if num_analyzed1 > num_analyzed2 then 1
    else 0

  method choose_working_block (penv: 't prog_env) (wblks: 't working_block list)
    : ('t working_block * 't working_block list) option =
    let compare wblk1 wblk2 : int =
      try
        (* let _ = compare_block_by_sparse_reachability penv wblk1 wblk2 |>
         *         (fun res -> if res != 0 then raise_int res) in
         * let _ = compare_block_by_name penv wblk1 wblk2 |>
         *         (fun res -> if res != 0 then raise_int res) in *)
        (* let _ = compare_block_by_num_pair_pred_succ_blocks penv wblk1 wblk2 |>
         *         (fun res -> if res != 0 then raise_int res) in
         * let _ = compare_block_by_distance_from_entry penv wblk1 wblk2 |>
         *         (fun res -> if res != 0 then raise_int res) in *)
        let _ = self#compare_block_by_analyzed_times penv wblk1 wblk2 |>
                (fun res -> if res != 0 then raise_int res) in
        (* let _ = compare_block_by_name penv wblk1 wblk2 |>
         *         (fun res -> if res != 0 then raise_int res) in *)
        0
      with EInt res -> res in
    let blks = List.sort ~compare wblks in
    match blks with
    | [] -> None
    | blk::nblks -> Some (blk, nblks)

  (** analyze all basic blocks using the MFP approach *)
  method analyze_blocks ?(widen=true) ?(fixpoint=true) (penv: 't prog_env) (fenv: 't func_env) wblks : unit =
    let prog = penv.penv_prog in
    let working_block, time = track_runtime (fun () -> self#choose_working_block penv wblks) in
    (* let _ = hprint "  Time choosing working block: " (sprintf "%.3fs") time in *)
    match working_block with
    | None -> ()
    | Some (wblk, nwblks) ->
        let (changed, continue), time =
          track_runtime (fun () -> self#analyze_block ~widen penv fenv wblk) in
        (* let _ = hprint " - Time analyzing block: " (sprintf "%.3fs") time in *)
        let nwblks =
          if changed && continue && fixpoint then
            (* let sblks = get_succeeding_blocks prog wblk.wb_block |>
             *             List.map ~f:(fun sb -> sb.sblk_block) in *)
            let sblks, time = track_runtime (fun () ->
                self#get_sparse_succeeding_blocks penv wblk.wb_block) in
            (* let _ = hprint " - Time getting sparse succeeding blocks: "
             *           (sprintf "%.3fs") time in *)
            let res, time = track_runtime (fun () ->
                List.fold_left ~f:(fun acc sb ->
                    match first_instr_of_block sb with
                    | None -> acc
                    | Some instr ->
                        let input, time = track_runtime (fun () ->
                            self#compute_block_input penv fenv sb) in
                        (* let _ = hprint ("- Time computing input of block : " ^ (block_name sb))
                         *           (sprintf "%.3fs") time in *)
                        let wb = self#mk_working_block sb instr input in
                        let _ = hdebug "--> Enqueue block: " block_name sb in
                        List.insert_dedup acc wb ~equal:self#equal_working_block) ~init:nwblks sblks) in
            (* let _ = hprint " - Time enqueue sparse blocks: " (sprintf "%.3fs") time in *)
            res
          else nwblks in
        self#analyze_blocks ~widen ~fixpoint penv fenv nwblks

  method do_widening_narrowing (penv: 't prog_env) (fenv: 't func_env) func : unit =
    let do_widening () =
      let entry = entry_block func in
      let wblocks = match fenv.fenv_working_blocks with
        | [] -> (match first_instr_of_block entry with
            | None -> []
            | Some instr ->
                let input = self#compute_block_input penv fenv entry in
                [self#mk_working_block entry instr input])
        | wbs -> wbs in
      let _ = fenv.fenv_working_blocks <- [] in
      self#analyze_blocks ~widen:true ~fixpoint:true penv fenv wblocks in
    let do_narrowing () =
      let wblocks = fold_left_blocks ~f:(fun acc blk ->
          match first_instr_of_block blk with
          | None -> acc
          | Some instr ->
              let input = self#compute_block_input penv fenv blk in
              acc @ [self#mk_working_block blk instr input]) ~init:[] func in
      self#analyze_blocks ~widen:false ~fixpoint:false penv fenv wblocks in
    let _ =
      let _ = debug ~ruler:`Short "Do widening..." in
      let _ = do_widening () in
      hdebug ~ruler:`Short "After widening:\n" (self#pr_func_env penv) fenv in
    let _ =
      let _ = debug ~ruler:`Short "Do narrowing..." in
      (* let _ = do_narrowing () in *)
      hdebug ~ruler:`Short "After narrowing:\n" (self#pr_func_env penv) fenv in
    ()

  method do_simple_fixpoint (penv: 't prog_env) (fenv: 't func_env) func : unit =
    let do_fixpoint () =
      let entry = entry_block func in
      let blocks = match fenv.fenv_working_blocks with
        | [] -> (match first_instr_of_block entry with
            | None -> []
            | Some instr ->
                let input = self#compute_block_input penv fenv entry in
                [self#mk_working_block entry instr input])
        | wbs -> wbs in
      (* let _ = hprint "  Time choosing blocks to start analyze: " (sprintf "%.3fs") time in *)
      let _ = fenv.fenv_working_blocks <- [] in
      self#analyze_blocks ~widen:false ~fixpoint:true penv fenv blocks in
    let _ = debug ~ruler:`Short "Do simple fixpoint..." in
    let _ = do_fixpoint () in
    (* let _ = hprint "  Time running simple fixpoint: " (sprintf "%.3fs") time in *)
    hdebug ~ruler:`Short "After fixpoint:\n\n" (self#pr_func_env penv) fenv

  (** analyze one function *)
  method analyze_function (penv: 't prog_env) (wf: 't working_func) : 't func_env * bool  * bool * bool * bool =
    let prog = penv.penv_prog in
    let func, input, callsites = wf.wf_func, wf.wf_input, wf.wf_callsites in
    let fname_params = func_name_and_params func in
    (* prepare environment *)
    let fenv = match self#find_func_env_by_input penv func input with
      | None -> self#mk_func_env prog func input ~callsites
      | Some fenv -> fenv in
    (* let nfenv = copy_func_env fenv in
     * let _ = reset_analysis_env penv nfenv in
     * nfenv in *)
    let _ = self#pre_analyze_func penv fenv in
    let _ = (* if not !print_concise_output then *)
      let msg = "Analyzing function: " ^ (fname_params) ^
                " ~ (" ^ (pr_int (self#get_func_analyzed_times penv func)) ^ ")" in
      print ~ruler:`Medium msg in
    let _ = if !mode_debug_function then
        let regex = Str.regexp !regex_debug_function in
        if Str.string_match regex (func_name func) 0 then
          let answer = ask_decision "Do you want to debug this function?" ["y"; "n"] in
          let answer = String.uppercase answer in
          if String.equal answer "Y" then
            let _ = save_mode_debug () in
            enable_mode_debug () in
    (* let _ = hdebug ~always:true " - Callsites: " (pr_list pr_callsite) callsites in *)
    let _ = hprint " - Callsites: " (pr_list self#pr_callsite) callsites in
    (* let _ = hdebug ~always:true " - Input: " pr_data input in *)
    let _ = hprint " - Input: " self#pr_data input in
    (* let _ = hprint " - Input: " pr_data_with_checksum input in *)
    (* let _ =
     *   if String.equal (func_name func) "free_a_tree" then
     *     hprint " - Input: " pr_data_with_checksum input
     *   else
     *     hprint " - Input: " pr_data input in *)
    let _ = hdebug ~always:true " - Re-analyzing from working blocks: " self#pr_working_blocks
              fenv.fenv_working_blocks in
    let _ = LP.get_loops_of_func prog func in
    let _ = self#update_func_analyzed_stats penv func in
    let _, time = track_runtime (fun () ->
        if self#need_widening func then self#do_widening_narrowing penv fenv func
        else self#do_simple_fixpoint penv fenv func) in
    (* let _ = hprint "  Time running iterative DFA: " (sprintf "%.3fs") time in *)
    let input_updated, output_updated, env_updated, env_completed =
      self#record_new_func_env penv fenv in
    let need_reanalyze = (env_updated && has_call_to_user_funcs prog func) in
    (* let _ = if not env_updated && env_completed then *)
    let _ = if (* not env_updated && *) env_completed then
        self#record_func_analyzed_input penv fenv input in
    let _ = if env_completed && fenv.fenv_output == None then
        fenv.fenv_output <- Some (least_data) in
    let _ = if not (is_func_main func) && fenv.fenv_output != None then
        let fsum, time = track_runtime (fun () -> self#mk_func_summary penv fenv input) in
        (* let _ = hprint "  Time creating func_summary: " (sprintf "%.3fs") time in *)
        let _ = hdebug ~ruler:`Short ("Analysis output: " ^ fname_params ^ ":\n\n")
                  self#pr_func_summary fsum in
        self#record_func_summary penv fsum in
    let _ = if !mode_debug_function then
        restore_mode_debug () in
    let _ = self#post_analyze_func penv fenv in
    (fenv, input_updated, output_updated, env_updated, need_reanalyze)

  method compare_func_callee_distance (penv: 't prog_env) (wf1: 't working_func) (wf2: 't working_func) : int =
    let prog = penv.penv_prog in
    let ig = prog.prog_instr_graph in
    let f1, f2 = wf1.wf_func, wf2.wf_func in
    let callers1 = get_func_callers prog f1 in
    let callers2 = get_func_callers prog f2 in
    let callers = List.filter ~f:(List.mem callers2 ~equal:equal_func) callers1 in
    let is_func_called_ealier g1 g2 = List.exists ~f:(fun caller ->
        let distance1 = LG.shortest_callee_distance ig caller g1 in
        let distance2 = LG.shortest_callee_distance ig caller g2 in
        match distance1, distance2 with
        | None, None -> false
        | Some _, None -> true
        | None, Some _ -> false
        | Some i1, Some i2 -> i1 <= i2) callers in
    let is_f1_called_ealier = is_func_called_ealier f1 f2 in
    let is_f2_called_ealier = is_func_called_ealier f2 f1 in
    if is_f1_called_ealier && not is_f2_called_ealier then -1
    else if not is_f1_called_ealier && is_f2_called_ealier then 1
    else 0

  method compare_func_analyzed_times (penv: 't prog_env) (wf1: 't working_func) (wf2: 't working_func) : int =
    let f1, f2 = wf1.wf_func, wf2.wf_func in
    let n1 = Hashtbl.find_default penv.penv_func_analyzed_times f1 ~default:0 in
    let n2 = Hashtbl.find_default penv.penv_func_analyzed_times f2 ~default:0 in
    if n1 < n2 then -1
    else if n1 > n2 then 1
    else 0

  method compare_func_main (penv: 't prog_env) (wf1: 't working_func) (wf2: 't working_func) : int =
    let f1, f2 = wf1.wf_func, wf2.wf_func in
    let n1 = Hashtbl.find_default penv.penv_func_analyzed_times f1 ~default:0 in
    let n2 = Hashtbl.find_default penv.penv_func_analyzed_times f2 ~default:0 in
    if is_func_main f1 && not (is_func_main f2) && n1 > 0 then 1
    else if not (is_func_main f1) && is_func_main f2 && n2 > 0 then -1
    else 0

  method compare_func_call_order (penv: 't prog_env) (wf1: 't working_func) (wf2: 't working_func) : int =
    let prog = penv.penv_prog in
    let f1, f2 = wf1.wf_func, wf2.wf_func in
    let callers1 = get_func_callers prog f1 in
    let callers2 = get_func_callers prog f2 in
    let is_f1_caller_f2 = List.mem ~equal:equal_func callers2 f1 in
    let is_f2_caller_f1 = List.mem ~equal:equal_func callers1 f2 in
    if not is_f1_caller_f2 && is_f2_caller_f1 then -1
    else if is_f1_caller_f2 && not is_f2_caller_f1 then 1
    else 0

  method compare_func_call_stack (penv: 't prog_env) (wf1: 't working_func) (wf2: 't working_func) : int =
    let prog = penv.penv_prog in
    let f1, f2 = wf1.wf_func, wf2.wf_func in
    match Stack.top penv.penv_func_analysis_stack with
    | None -> 0
    | Some f ->
        let callees = get_func_callees prog f in
        let is_f1_callee = List.mem callees f1 ~equal:equal_func in
        let is_f2_callee = List.mem callees f2 ~equal:equal_func in
        if is_f1_callee && not is_f2_callee then -1
        else if is_f2_callee && not is_f1_callee then 1
        else 0

  method compare_func_input (penv: 't prog_env) (wf1: 't working_func) (wf2: 't working_func) : int =
    let prog = penv.penv_prog in
    let f1, f2 = wf1.wf_func, wf2.wf_func in
    if equal_func f1 f2 then
      let input1, input2 = wf1.wf_input, wf2.wf_input in
      if self#lequal_data input1 input2 then -1
      else if self#lequal_data input2 input1 then 1
      else 0
    else 0

  method choose_best_working_func (penv: 't prog_env) wfuncs : ('t working_func * 't working_func list) option =
    let wfunc_opt = List.find ~f:(fun wf1 ->
      List.for_all ~f:(fun wf2 ->
        if not (self#equal_working_func wf1 wf2) then
          let n1 = self#compare_func_main penv wf1 wf2 in
          let n2 = self#compare_func_call_order penv wf1 wf2 in
          let n3 = self#compare_func_call_stack penv wf1 wf2 in
          (* let n4 = compare_func_callee_distance penv wf1 wf2 in *)
          let n5 = self#compare_func_analyzed_times penv wf1 wf2 in
          n1 <= 0 && n2 <= 0 && n3 <= 0 && (* n4 <= 0 && n5 <= 0 && *)
          n1 + n2 + n3 (* + n4 + n5 *) < 0
        else true
      ) wfuncs
    ) wfuncs in
    match wfunc_opt with
    | None -> List.extract_nth 0 wfuncs
    | Some wf ->
      let others_wfs = List.exclude ~f:(self#equal_working_func wf) wfuncs in
      Some (wf, others_wfs)

  method choose_working_func (penv: 't prog_env) : ('t working_func * 't working_func list) option =
    let compare (wf1: 't working_func) (wf2: 't working_func) : int =
      try
        let _ = self#compare_func_main penv wf1 wf2 |>
                (fun res -> if res != 0 then raise_int res) in
        let _ = self#compare_func_call_order penv wf1 wf2 |>
                (fun res -> if res != 0 then raise_int res) in
        let _ = self#compare_func_call_stack penv wf1 wf2 |>
                (fun res -> if res != 0 then raise_int res) in
        (* let _ = compare_func_callee_distance penv wf1 wf2 |>
         *         (fun res -> if res != 0 then raise_int res) in *)
        let _ = self#compare_func_analyzed_times penv wf1 wf2 |>
                (fun res -> if res != 0 then raise_int res) in
        (* let _ = compare_func_input penv wf1 wf2 |>
         *         (fun res -> if res != 0 then raise_int res) in *)
        0
      with EInt res -> res in
    let wfuncs = List.sorti ~compare penv.penv_working_funcs in
    (* let _ = hdebug ~always:false "Analyzed functions: " pr_analyzed_func_input penv in *)
    match is_interactive_mode () with
    | true ->
        let choices = display_choices "Working functions: " self#pr_working_func wfuncs in
        let decicion = ask_decision "Choose a function: " ["a"; "q"; choices] in
        if String.equal decicion "a" then List.extract_nth 0 wfuncs
        else if String.equal decicion "q" then None
        else List.extract_nth ((int_of_string decicion) - 1) wfuncs
    | false ->
        let _ = hdebug "\nWorking functions (after sorting): " self#pr_working_funcs wfuncs  in
        (* let _ = hprint "\n#Working functions: " pr_int (List.length wfuncs) in *)
        (* let _ = hdebug ~always:true "Working functions (after sorting): " pr_working_funcs wfuncs  in *)
        (* choose_best_working_func penv wfuncs *)
        List.extract_nth 0 wfuncs

  (** analyze all functions, using the MFP approach *)
  method analyze_functions (penv: 't prog_env) : unit =
    let prog = penv.penv_prog in
    match self#choose_working_func penv with
    | None -> ()
    | Some (wf, other_wfs) ->
        let _ = penv.penv_working_funcs <- other_wfs in
        let func, input, callsites = wf.wf_func, wf.wf_input, wf.wf_callsites in
        let fname = func_name func in
        let (fenv, input_updated, output_updated, env_updated, need_reanalyze), time =
          track_runtime (fun () -> self#analyze_function penv wf) in
        let _ = hdebug (" - Time analyzing function: " ^ fname ^ ": ")
                  (sprintf "%.3fs") time in
        (* let _ = if Float.(>) time 15. then
         *     hprint "TOO SLOW... Func env: " (pr_func_env penv) fenv in *)
        let _ = hdebug "Analysis output updated: " pr_bool env_updated in
        let _ =
          if need_reanalyze then
            let _ = debug ~always:true ("Prepare to enqueue for reanalyze: " ^ fname) in
            self#enqueue_to_analyze_func ~msg:"reanalyze itself" penv wf
          else hdebug "Complete analyzing working function: " self#pr_working_func wf in
        (* let _ = if input_updated || env_updated then *)
        (* let _ = if input_updated || env_updated || output_updated then *)
        let _ = (* if input_updated || env_updated then *)
          List.iter ~f:(fun cs ->
              let caller, caller_input = cs.cs_caller, cs.cs_caller_input in
              let caller_callsites = cs.cs_caller_callsites in
              let wf_caller = self#mk_working_func caller caller_input caller_callsites in
              let _ = hdebug ~always:true "Prepare to enqueue caller: " func_name
                        wf_caller.wf_func in
              self#enqueue_to_analyze_func ~msg:"caller" penv wf_caller) wf.wf_callsites in
        self#analyze_functions penv

  (** analyze global variables *)
  method analyze_globals (penv: 't prog_env) : unit =
    let prog = penv.penv_prog in
    let genv = penv.penv_global_env in
    let _ = debug ~ruler:`Medium "Analyzing globals" in
    let input = least_data in
    let globals = prog.prog_globals in
    let output = List.fold_left ~f:(fun input global ->
        if not (self#is_sparse_global penv global) then input
        else
          let _ = hdebug "  " pr_global global in
          let _ = hdebugc "    In:  " self#pr_data input in
          let output = self#analyze_global global input in
          let _ = self#set_global_output genv global output in
          let _ = hdebugc "    Out: " self#pr_data output in
          output) globals  ~init:input in
    let output =
      let func_init_global = List.find ~f:(fun f ->
          String.equal (func_name f) __init_globals) prog.prog_init_funcs in
      match func_init_global with
      | None -> output
      | Some gfunc ->
          let _ = genv.genv_globals_data <- output in
          let ginput = output in
          let wf_global = self#mk_working_func gfunc ginput [] in
          let _ = self#enqueue_to_analyze_func ~msg:"init" penv wf_global in
          let _ = self#analyze_functions penv in
          let gfenv = self#find_func_env_by_input penv gfunc ginput in
          match gfenv with
          | None -> herror "analyze_globals: fenv not found: " func_name gfunc
          | Some fenv -> match fenv.fenv_output with
            | None -> herror "analyze_globals: output not found: " func_name gfunc
            | Some output -> self#clean_irrelevant_info_from_data penv gfunc output  in
    genv.genv_globals_data <- output

  method init_sparse_blocks (penv: 't prog_env) : unit =
    List.iter ~f:(fun func ->
        iter_blocks ~f:(fun blk ->
            if not (Hashtbl.mem penv.penv_sparse_block blk) ||
               self#is_sparse_block penv blk then
              if is_first_block_of_func blk then
                Hashtbl.set penv.penv_sparse_block ~key:blk ~data:true
              else
                let num_instrs = fold_left_instrs ~f:(fun acc instr ->
                    if self#is_sparse_instr penv instr then acc + 1
                    else acc) ~init:0 blk in
                let num_sblks = blk |> get_succeeding_blocks penv.penv_prog |>
                                List.length in
                if num_instrs = 0 then
                  Hashtbl.set penv.penv_sparse_block ~key:blk ~data:false
                else if num_instrs = 1 && num_sblks = 1 then
                  Hashtbl.set penv.penv_sparse_block ~key:blk ~data:false
                else Hashtbl.set penv.penv_sparse_block ~key:blk ~data:true
          ) func
      ) penv.penv_goal_funcs

  method refine_sparse_blocks (penv: 't prog_env) : bool =
    let updated = ref false in
    let prog = penv.penv_prog in
    let equal = equal_block in
    let get_sparse_block_reachable_from_entry func : block list =
      let rec get_blocks blks res : block list =
        match blks with
        | [] -> res
        | blk::nblks ->
            let nres = res @ [blk] in
            let sblks = self#get_sparse_succeeding_blocks ~cache:false penv blk |>
                        List.filter ~f:(List.not_mem nres ~equal) in
            let nblks = List.concat_dedup nblks sblks ~equal in
            get_blocks nblks nres in
      match first_block_of_func func with
      | None -> []
      | Some blk -> get_blocks [blk] [] in
    let _ = List.iter ~f:(fun func ->
        let sparse_blks = get_sparse_block_reachable_from_entry func in
        iter_blocks ~f:(fun blk ->
            match Hashtbl.find penv.penv_sparse_block blk with
            | Some true when List.not_mem sparse_blks blk ~equal ->
                let _ = updated := true in
                Hashtbl.set penv.penv_sparse_block ~key:blk ~data:false
            | _ -> ()) func) penv.penv_goal_funcs in
    !updated

  method init_sparse_funcs (penv: 't prog_env) : unit =
    List.iter ~f:(fun f ->
        if not (Hashtbl.mem penv.penv_sparse_func f) ||
           self#is_sparse_func penv f then
          let num_sparse_instrs = ref 0 in
          let has_return_or_unreachable = ref false in
          let has_return_of_non_pointer = ref false in
          let finstr = Some (fun instr ->
              if self#is_sparse_instr penv instr then
                match instr_opcode instr with
                | LO.Br | LO.IndirectBr | LO.Switch -> ()
                | LO.Unreachable | LO.Ret ->
                    let _ = num_sparse_instrs := !num_sparse_instrs + 1 in
                    let _ = has_return_or_unreachable := true in
                    if is_instr_return instr &&
                       not (is_llvalue_pointer (src_of_instr_return instr)) then
                      has_return_of_non_pointer := true
                | _ -> num_sparse_instrs := !num_sparse_instrs + 1) in
          let _ = deep_iter_func ~finstr f in
          if !num_sparse_instrs == 0 || not !has_return_or_unreachable ||
             (!num_sparse_instrs == 1 && !has_return_of_non_pointer) then
            let _ = Hashtbl.set penv.penv_sparse_func ~key:f ~data:false in
            let vf = llvalue_of_func f in
            LL.iter_uses (fun u ->
                Hashtbl.set penv.penv_sparse_llvalue ~key:(LL.user u) ~data:false) vf
          else Hashtbl.set penv.penv_sparse_func ~key:f ~data:true
      ) penv.penv_goal_funcs

  method refine_sparse_funcs (penv: 't prog_env) : bool =
    let updated = ref false in
    let prog = penv.penv_prog in
    let rec mark_non_sparse_funcs penv =
      let continue = ref false in
      let _ = List.iter ~f:(fun f ->
          (* let _ = hprint "Refine function : " func_name f in *)
          if self#is_sparse_func penv f then
            let has_pointer_related_instr = ref false in
            let finstr = Some (fun instr ->
                if self#is_sparse_instr penv instr then
                  match instr_opcode instr with
                  | LO.Br | LO.IndirectBr | LO.Switch | LO.Unreachable -> ()
                  | LO.Ret ->
                      if is_llvalue_pointer (src_of_instr_return instr) then
                        has_pointer_related_instr := true
                  | LO.Call | LO.Invoke ->
                      let callee = callee_of_instr_func_call instr in
                      if not (is_func_pointer callee) &&
                         not (self#is_sparse_func penv callee) then ()
                      else has_pointer_related_instr := true
                  | _ -> has_pointer_related_instr := true) in
            let _ = deep_iter_func ~finstr f in
            if not !has_pointer_related_instr then
              (* let _ = hprint "Set function to non-sparse: " func_name f in *)
              let _ = Hashtbl.set penv.penv_sparse_func ~key:f ~data:false in
              let _ = continue := true in
              let _ = updated := true in
              let vf = llvalue_of_func f in
              LL.iter_uses (fun u ->
                  (* let _ = hprint "  User: " pr_value_detail (LL.user u) in *)
                  Hashtbl.set penv.penv_sparse_llvalue ~key:(LL.user u) ~data:false) vf
        ) penv.penv_goal_funcs in
      if !continue then mark_non_sparse_funcs penv in
    let _ = mark_non_sparse_funcs penv in
    !updated

  method compute_sparse_used_globals (penv: 't prog_env) : unit =
    let _ = print "Compute sparse used globals" in
    let prog = penv.penv_prog in
    let tbl_used_globals = penv.penv_sparse_used_globals in
    let equal = equal_llvalue in
    let init_globals_of_all_funcs () =
      let _ = List.iter ~f:(fun f ->
          let gs = f |> get_func_used_globals prog |>
                   List.filter ~f:(self#is_sparse_global penv) in
          Hashtbl.set tbl_used_globals ~key:f ~data:gs) prog.prog_init_funcs in
      List.iter ~f:(fun func ->
          let gs = ref [] in
          let finstr = Some (fun instr ->
              if self#is_sparse_instr penv instr then
                for i = 0 to (num_operands instr) - 1 do
                  let opr = operand instr i in
                  match LL.classify_value opr with
                  | LV.GlobalVariable ->
                      gs := List.insert_sorti_dedup !gs (mk_global opr) ~compare:Poly.compare
                  | _ -> ()
                done) in
          let _ = deep_iter_func ~finstr func in
          Hashtbl.set tbl_used_globals ~key:func ~data:!gs) prog.prog_user_funcs in
    let update_globals_of_all_funcs () =
      let funcs = prog.prog_init_funcs @ prog.prog_user_funcs in
      let rec update_globals fs acc =
        let compare = Poly.compare in
        match fs with
        | [] -> acc
        | f::nfs ->
            (* let _ = hprint "Update globals of func: " func_name f in *)
            let gs = Hashtbl.find_default tbl_used_globals f ~default:[] in
            let ngs = ref gs in
            let callees =
              let callees = get_func_callees prog f in
              if not !dfa_used_globals_in_func_ptrs then callees
              else
                let ptr_callees = get_func_ptr_callees prog f in
                if List.is_empty ptr_callees then callees
                else
                  List.fold_left ~f:(fun acc pc ->
                      let ftyp = LL.type_of pc in
                      match Hashtbl.find prog.prog_funcs_in_pointers ftyp with
                      | None -> acc
                      | Some fs -> List.concat_dedup acc fs ~equal:equal_func
                    ) ptr_callees ~init:callees in
            let cgs = List.fold_left ~f:(fun acc f1 ->
                let gs1 = Hashtbl.find_default tbl_used_globals f1 ~default:[] in
                List.concat_sorti_dedup acc gs1 ~compare) ~init:[] callees in
            let _ = ngs := List.concat_sorti_dedup !ngs cgs ~compare in
            (* let _ = print "  done" in *)
            if List.length !ngs > List.length gs then
              let _ = Hashtbl.set tbl_used_globals ~key:f ~data:!ngs in
              update_globals nfs true
            else update_globals nfs acc in
      let updated = ref true in
      while (!updated) do
        updated := update_globals funcs false
      done in
    let _ = init_globals_of_all_funcs () in
    update_globals_of_all_funcs ()

  method initialize_analysis (penv: 't prog_env) func =
    let prog = penv.penv_prog in
    let _ = if !export_core_prog then self#export_core_program_to_file ~sparse:false penv in
    let _ = penv.penv_goal_funcs <- prog.prog_init_funcs @ prog.prog_user_funcs in
    let _ = record_runtime (fun () ->
        (* TODO: turn this into a fix-point function *)
        let _ = if !dfa_sparse_analysis then (
            let _ = self#init_sparse_globals_instrs penv in
            let _ = self#init_sparse_funcs penv in
            let continue = ref true in
            let _ = while !continue do (
                let _ = continue := false in
                let _ =
                  let _ = self#init_sparse_blocks penv in
                  if self#refine_sparse_blocks penv then continue := true in
                let _ =
                  let _ = self#init_sparse_funcs penv in
                  if self#refine_sparse_funcs penv then continue := true in
                if self#refine_sparse_globals_instrs penv then continue := true;
              ) done in
            self#compute_sparse_used_globals penv) in
        let new_goals = List.filter ~f:(self#is_sparse_func penv) penv.penv_goal_funcs in
        penv.penv_goal_funcs <- new_goals) sparse_time in
    let _ = hprint " - Time preparing sparse program: " (sprintf "%.3fs") !sparse_time in
    (* let _ = hprint "Non-sparse functions: "  *)
    let _ = if !print_stats_prog then
        self#print_stats_sparse_prog penv in
    let _ = hprint "Goal functions: " (pr_items func_name) penv.penv_goal_funcs in
    (* let prog = update_program_info prog in *)
    let _ = if !export_core_prog then
        let _ = self#export_core_program_to_file penv in
        self#export_core_program_to_file ~sparse:true penv in
    let _ = if !export_debug_info then self#export_debugging_info_to_file penv in
    if !dfa_sparse_analysis && not !print_concise_output then
      hprint ~ruler:`Header "CORE SPARSE PROGRAM" self#pr_sparse_prog penv

  method analyze_program_intraproc ?(func=None) (penv: 't prog_env) : 't prog_env =
    let prog = penv.penv_prog in
    let funcs = match func with
      | Some f -> [f]
      | None -> prog.prog_user_funcs in
    let _ = self#analyze_globals penv in
    let _ = List.iter ~f:(fun f ->
        (* prepare environment and input *)
        let _ = self#initialize_analysis penv f in
        let input = penv.penv_global_env.genv_globals_data in
        let wf = self#mk_working_func f input [] in
        (* then analyze *)
        let _ = self#analyze_function penv wf in
        ()) funcs in
    penv

  method analyze_program_interproc ?(func=None) (penv: 't prog_env) : 't prog_env =
    let prog = penv.penv_prog in
    let func = match func with
      | Some f -> f
      | None -> match prog.prog_main_func with
        | Some f -> f
        | None -> error "analyze_program_interproc: entry function not found!" in
    (* then analyze *)
    let _ = hprint "Entry function: " func_name func in
    let _ = self#initialize_analysis penv func in
    let _ = self#analyze_globals penv in
    let input = penv.penv_global_env.genv_globals_data in
    let wf = self#mk_working_func func input [] in
    let _ = self#enqueue_to_analyze_func ~msg:"entry" penv wf in
    let _ = self#analyze_functions penv in
    penv

  method analyze_program ?(interproc=false) (prog: program) : 't prog_env =
    let penv = self#mk_prog_env prog in
    let func = match !dfa_func_name with
      | Some fname -> find_user_func prog fname
      | None -> None in
    let _ = self#pre_analyze_prog penv in
    let penv = match !dfa_mode with
      | DfaIntraProc -> self#analyze_program_intraproc ~func penv
      | DfaInterProc -> self#analyze_program_interproc ~func penv in
    let _ = self#post_analyze_prog penv in
    penv

  (* bug checking *)

  method check_bug_prog (penv: 't prog_env) (bug: BG.bug) : ternary =
    let func = bug.BG.bug_func in
    match Hashtbl.find penv.penv_func_envs func with
    | None -> Unkn
    | Some fenvs ->
        List.fold_left ~f:(fun acc fenv ->
            match acc with
            | True -> True
            | Unkn ->
                let res = self#check_bug fenv bug in
                if res == False then acc
                else res
            | False -> self#check_bug fenv bug) ~init:False fenvs

  method check_assertions (penv: 't prog_env) : unit =
    let prog = penv.penv_prog in
    let num_total = self#count_assertions prog in
    let funcs = Hashtbl.keys penv.penv_func_envs in
    let num_checked = List.fold ~f:(fun acc func ->
        acc + (self#check_func_assertions penv func)) ~init:0 funcs in
    let num_skipped = num_total - num_checked in
    if (num_total = 0) then print "No assertion is found!"
    else
      let msg =
        let info = (pr_int num_checked) ^ "/" ^ (pr_int num_total) ^
                   " assertion(s) are checked" in
        if num_skipped == 0 then info ^ "!"
        else info ^ ", " ^ (pr_int num_skipped) ^ " are skipped!" in
      println msg

  method report_analysis_stats (penv: 't prog_env) : unit =
    (* if not !print_concise_output || !print_concise_debug then *)
    println ~format:false (* ~always:true *)
      ("\nStatistics of " ^ (name_of_dfa analysis) ^ ": \n" ^
       (self#pr_analysis_stats penv))
end
