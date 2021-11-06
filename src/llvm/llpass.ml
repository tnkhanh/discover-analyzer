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
open Llir
module LL = Llvm
module LO = Llvm.Opcode
module LD = Lldebug
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph

let construct_map_llvalue_to_source_name (prog : program) : unit =
  let _ = ddebugc "Construct mapping llvalue to source name" in
  let metadata = ref [] in
  let finstr =
    Some
      (fun instr ->
        match instr_opcode instr with
        | LO.Call | LO.Invoke ->
          if is_func_llvm_debug (callee_of_instr_func_call instr)
          then (
            let _ = hprint "instr: " pr_instr instr in
            let v0, v1 = operand instr 0, operand instr 1 in
            let mdv0, mdv1 = LL.value_as_metadata v0, LL.value_as_metadata v1 in
            let _ = hprint "opr 0: " LL.value_name (operand instr 0) in
            let _ = hprint "opr 1: " LL.value_name (operand instr 1) in
            let _ = hprint "opr 0: " LL.string_of_llvalue (operand instr 0) in
            let _ = hprint "opr 1: " LL.string_of_llvalue (operand instr 1) in
            let vname = pr_value (operand instr 0) in
            let sname = LD.extract_name_from_metadata (operand instr 1) in
            Hashtbl.set prog.prog_llvalue_original_name ~key:vname ~data:sname)
          else ()
        | _ -> ()) in
  deep_iter_program ~finstr prog
;;

let compute_func_call_info (prog : program) : unit =
  let _ = ndebug "Compute function call information" in
  let equal = equal_func in
  let finstr =
    Some
      (fun instr ->
        match instr_opcode instr with
        | LO.Call | LO.Invoke ->
          let callee = callee_of_instr_func_call instr in
          let vcallee = llvalue_of_func callee in
          let caller = func_of_instr instr in
          if is_llvalue_function vcallee
          then (
            let callers =
              List.insert_dedup (get_func_callers prog callee) caller ~equal
            in
            let _ =
              Hashtbl.set prog.prog_func_callers ~key:callee ~data:callers in
            let callees =
              List.insert_dedup (get_func_callees prog caller) callee ~equal
            in
            Hashtbl.set prog.prog_func_callees ~key:caller ~data:callees)
          else (
            let pcallees = get_func_ptr_callees prog caller in
            let pcallees =
              List.insert_dedup pcallees vcallee ~equal:equal_llvalue in
            Hashtbl.set prog.prog_func_ptr_callees ~key:caller ~data:pcallees)
        | _ -> ()) in
  deep_iter_program ~finstr prog
;;

let construct_func_call_graph (prog : program) : unit =
  Hashtbl.iteri
    ~f:(fun ~key:func ~data:callees ->
      List.iter ~f:(CG.add_edge prog.prog_func_call_graph func) callees)
    prog.prog_func_callees
;;

let compute_funcs_in_pointers (prog : program) : unit =
  let _ = ndebug "Compute functions in pointers" in
  let finstr =
    Some
      (fun instr ->
        let func =
          match instr_opcode instr with
          | LO.BitCast ->
            let v = src_of_instr_bitcast instr in
            if is_llvalue_function v then Some (mk_func v) else None
          | LO.Store ->
            let v = src_of_instr_store instr in
            if is_llvalue_function v then Some (mk_func v) else None
          | _ -> None in
        match func with
        | None -> ()
        | Some f ->
          let ftyp = type_of_func f in
          let curr_funcs =
            match Hashtbl.find prog.prog_funcs_in_pointers ftyp with
            | None -> []
            | Some fs -> fs in
          let nfuncs = List.insert_dedup curr_funcs f ~equal:equal_func in
          Hashtbl.set prog.prog_funcs_in_pointers ~key:ftyp ~data:nfuncs) in
  deep_iter_program ~finstr prog
;;

let compute_func_used_globals prog : unit =
  let _ = print "Compute used globals in each functions" in
  let tbl_used_globals = prog.prog_func_used_globals in
  let equal = equal_llvalue in
  let init_globals_of_all_funcs () =
    let _ =
      List.iter
        ~f:(fun func ->
          let gs = List.sorti prog.prog_globals ~compare:Poly.compare in
          Hashtbl.set tbl_used_globals ~key:func ~data:gs)
        prog.prog_init_funcs in
    List.iter
      ~f:(fun func ->
        let gs = ref [] in
        let finstr =
          Some
            (fun instr ->
              for i = 0 to num_operands instr - 1 do
                let opr = operand instr i in
                match LL.classify_value opr with
                | LV.GlobalVariable ->
                  gs
                    := List.insert_sorti_dedup
                         !gs
                         (mk_global opr)
                         ~compare:Poly.compare
                | _ -> ()
              done) in
        let _ = deep_iter_func ~finstr func in
        Hashtbl.set tbl_used_globals ~key:func ~data:!gs)
      prog.prog_user_funcs in
  let update_globals_of_all_funcs () =
    let funcs = prog.prog_init_funcs @ prog.prog_user_funcs in
    let rec update_globals fs acc =
      let compare = Poly.compare in
      match fs with
      | [] -> acc
      | f :: nfs ->
        (* let _ = hprint "Update globals of func: " func_name f in *)
        let gs = Hashtbl.find_default tbl_used_globals f ~default:[] in
        let ngs = ref gs in
        let callees =
          let callees = get_func_callees prog f in
          if not !dfa_used_globals_in_func_ptrs
          then callees
          else (
            let ptr_callees = get_func_ptr_callees prog f in
            if List.is_empty ptr_callees
            then callees
            else
              List.fold_left
                ~f:(fun acc pc ->
                  let ftyp = LL.type_of pc in
                  match Hashtbl.find prog.prog_funcs_in_pointers ftyp with
                  | None -> acc
                  | Some fs -> List.concat_dedup acc fs ~equal:equal_func)
                ptr_callees
                ~init:callees) in
        let cgs =
          List.fold_left
            ~f:(fun acc f1 ->
              let gs1 = Hashtbl.find_default tbl_used_globals f1 ~default:[] in
              List.concat_sorti_dedup acc gs1 ~compare)
            ~init:[]
            callees in
        let _ = ngs := List.concat_sorti_dedup !ngs cgs ~compare in
        (* let _ = print "  done" in *)
        if List.length !ngs > List.length gs
        then (
          let _ = Hashtbl.set tbl_used_globals ~key:f ~data:!ngs in
          update_globals nfs true)
        else update_globals nfs acc in
    let updated = ref true in
    while !updated do
      updated := update_globals funcs false
    done in
  let _ = init_globals_of_all_funcs () in
  update_globals_of_all_funcs ()
;;

let update_program_info (prog : program) : unit =
  let _ = print "Updating program information..." in
  let _ =
    Sys.report_runtime ~task:"Time computing funcs in pointers" (fun () ->
        compute_funcs_in_pointers prog) in
  let _ =
    Sys.report_runtime ~task:"Time computing func call info" (fun () ->
        compute_func_call_info prog) in
  let _ =
    Sys.report_runtime ~task:"Time computing used globals" (fun () ->
        compute_func_used_globals prog) in
  let _ =
    Sys.report_runtime ~task:"Time constructing callgraph" (fun () ->
        construct_func_call_graph prog) in
  let _ =
    Sys.report_runtime ~task:"Time constructing reachability graph" (fun () ->
        LG.build_reachability_graph prog) in
  (* let _ = construct_map_llvalue_to_source_name prog in *)
  ()
;;
