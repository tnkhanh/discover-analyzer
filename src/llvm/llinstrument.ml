(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Printer
open Debugger
open Llir
module LL = Llvm
module LO = Llvm.Opcode
module LD = Llvm_debuginfo
module LS = Lldebug
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph

exception AnnotFormat of string

type ann_type =
  | Bug
  | Safe

type instr_with_tag =
  { pos : position;
    tag : int;
    ins : instr
  }

let make_tagged_ins pos_ tag_ ins_ = { pos = pos_; tag = tag_; ins = ins_ }

let less_than ins ann =
  if Poly.((ins.pos.pos_line_end, ins.pos.pos_col_end) < Ann.pos_of_ann ann)
  then true
  else false
;;

type linecol =
  { line : int;
    col : int
  }

let make_linecol line_ col_ = { line = line_; col = col_ }

let lc_comp lcx lcy =
  let tuplex = lcx.line, lcx.col in
  let tupley = lcy.line, lcy.col in
  Poly.compare tuplex tupley
;;

let max_lc lcx lcy = if lc_comp lcx lcy = 1 then lcx else lcy
let min_lc lcx lcy = if lc_comp lcx lcy = 1 then lcy else lcx

let less_than ins ann =
  if Poly.((ins.pos.pos_line_end, ins.pos.pos_col_end) < Ann.pos_of_ann ann)
  then true
  else false
;;

let coverage = Hashtbl.create (module Instr)

let rec get_coverage instr =
  match Hashtbl.find coverage instr with
  | None ->
    let pos_op = LS.position_of_instr instr in
    let oprc = num_operands instr in
    let cover =
      let init =
        match pos_op with
        | None -> { line = max_int; col = max_int }, { line = 0; col = 0 }
        | Some pos ->
          ( make_linecol pos.pos_line_start pos.pos_col_start,
            make_linecol pos.pos_line_end pos.pos_col_end ) in
      let rec fold_opr acc idx =
        if idx = oprc
        then acc
        else (
          let opr_llvalue = operand instr idx in
          match LL.classify_value opr_llvalue with
          | LV.Instruction _ ->
            let opr = mk_instr opr_llvalue in
            let opr_cov_start, opr_cov_end = get_coverage opr in
            (match acc with
            | acc_start, acc_end ->
              let new_start = min_lc opr_cov_start acc_start in
              let new_end = max_lc opr_cov_end acc_end in
              fold_opr (new_start, new_end) (idx + 1))
          | _ -> fold_opr acc (idx + 1)) in
      fold_opr init 0 in
    let _ = Hashtbl.add coverage ~key:instr ~data:cover in
    cover
  | Some cover -> cover
;;

let extract_ann_marks (filename : string) =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let rev_mark_list =
    try Annparser.prog Annlexer.read lexbuf
    with Annparser.Error ->
      let _ =
        debug ("Parsing failed. Annotations ignored in file: " ^ filename)
      in
      [] in
  (*print all marks*)
  let _ = debug ~ruler:`Short "Annotations" in
  let _ =
    List.iter (List.rev (List.map rev_mark_list ~f:Ann.str_of_mark)) ~f:debug
  in
  List.rev rev_mark_list
;;

let get_func_name anntyp (bug : Ann.bug_group) ins_type =
  let base_name =
    match anntyp with
    | Bug ->
      (match bug with
      | MemoryLeak -> "__assert_memory_leak"
      | NullPointerDeref -> "__assert_null_pointer_deref"
      | BufferOverflow -> "__assert_buffer_overflow"
      | IntegerOverflow -> "__assert_bug_integer_overflow"
      | IntegerUnderflow -> "__assert_integer_underflow"
      | DivisionByZero -> "__assert_division_by_zero"
      | NewType t -> "__assert_unnamed_bug")
    | Safe ->
      (match bug with
      | MemoryLeak -> "__refute_memory_leak"
      | NullPointerDeref -> "__refute_null_pointer_deref"
      | BufferOverflow -> "__refute_buffer_overflow"
      | IntegerOverflow -> "__refute_bug_integer_overflow"
      | IntegerUnderflow -> "__refute_integer_underflow"
      | DivisionByZero -> "__refute_division_by_zero"
      | NewType t -> "__refute_unnamed_bug") in
  let tail = LL.string_of_lltype ins_type in
  base_name ^ "_" ^ tail
;;

let get_func_args (bug : Ann.bug_group) ins llctx =
  match bug with
  (*  | IntegerOverflow -> *)
  (*  let width = LL.integer_bitwidth (LL.type_of ins) in*)
  (*  [| ins; LL.const_int (LL.i32_type llctx) width |] *)
  | _ -> [| ins |]
;;

let apply_annotation
    (anntyp : ann_type)
    (instr : instr_with_tag)
    (bugs : Ann.bug_group list)
    (modul : llmodule)
  =
  match instr.ins with
  | Instr inx ->
    (* if ins is store and first operand is trunc *)
    let actual_ins =
      if is_instr_store instr.ins
      then (
        let first_opr = operand instr.ins 0 in
        match LL.instr_opcode first_opr with
        | LO.Trunc -> first_opr
        | _ -> inx)
      else inx in
    let insert_pos = LL.instr_succ actual_ins in
    let llctx = LL.module_context modul in
    let builder = LL.builder_at llctx insert_pos in
    let ins_type = LL.type_of actual_ins in
    List.iter bugs ~f:(fun bug ->
        let func_name = get_func_name anntyp bug ins_type in
        let assert_func_opt = LL.lookup_function func_name modul in
        let assert_func =
          match assert_func_opt with
          | None ->
            let func_type =
              LL.function_type (LL.void_type llctx) [| ins_type |] in
            let func = LL.declare_function func_name func_type modul in
            func
          | Some func -> func in
        let args = get_func_args bug actual_ins llctx in
        let _ = LL.build_call assert_func args "" builder in
        ())
;;

let rec update (ins : instr_with_tag) anns =
  match anns with
  | [] -> []
  | hd_match :: tl ->
    (match hd_match with
    | ann, ins_op ->
      (match ins_op with
      | None -> (ann, Some ins) :: update ins tl
      | Some old_ins ->
        let choose_ins =
          let old_start, old_end = get_coverage old_ins.ins in
          let cstart, cend = get_coverage ins.ins in
          let start_comp = lc_comp old_start cstart in
          let end_comp = lc_comp old_end cend in
          if start_comp < 0
             || (start_comp = 0 && end_comp > 0)
             || (start_comp = 0 && end_comp = 0 && old_ins.tag < ins.tag)
          then old_ins
          else ins in
        (ann, Some choose_ins) :: update ins tl))
;;

let apply_hd_bug
    end_ann
    (matched_anns : (Ann.mark * instr_with_tag option) list)
    modul
  =
  match matched_anns with
  | [] ->
    raise
      (AnnotFormat
         ("Error: Bug_end without start at " ^ Ann.pr_pos_ann end_ann))
  | (ann, ins_op) :: tl ->
    (match ann with
    | Bug_start ((line, col), bugs) ->
      (match ins_op with
      | None ->
        raise
          (AnnotFormat
             ("Error: No ins for annot at " ^ Ann.pr_pos_ann end_ann))
      | Some ins ->
        let _ = apply_annotation Bug ins bugs modul in
        tl)
    | Bug_end _ | Safe_start _ | Safe_end _ | Skip ->
      raise
        (AnnotFormat
           ("Error: no Bug_start matching Bug_end at "
           ^ Ann.pr_pos_ann end_ann)))
;;

let apply_hd_safe
    end_ann
    (matched_anns : (Ann.mark * instr_with_tag option) list)
    modul
  =
  match matched_anns with
  | [] ->
    raise
      (AnnotFormat
         ("Error: Safe_end without start at " ^ Ann.pr_pos_ann end_ann))
  | (ann, ins_op) :: tl ->
    (match ann with
    | Safe_start ((line, col), bugs) ->
      (match ins_op with
      | None ->
        raise
          (AnnotFormat
             ("Error: No ins for annot at " ^ Ann.pr_pos_ann end_ann))
      | Some ins ->
        let _ = apply_annotation Safe ins bugs modul in
        tl)
    | Bug_start _ | Bug_end _ | Safe_end _ | Skip ->
      raise
        (AnnotFormat
           ("Error: no Safe_start matching Safe_end at"
           ^ Ann.pr_pos_ann end_ann)))
;;

let rec resolve
    (ann_marks : Ann.mark list)
    (instrs : instr_with_tag list)
    matched_anns
    modul
  =
  match ann_marks with
  | [] -> ()
  | hd_ann :: tl_anns ->
    (match hd_ann with
    | Bug_start ((line, col), bugs) | Safe_start ((line, col), bugs) ->
      (match instrs with
      | [] ->
        raise
          (AnnotFormat
             ("Error: no matching instruction for bug annotation at "
             ^ pr_int line
             ^ " "
             ^ pr_int col))
      | ins :: tl_ins ->
        if less_than ins hd_ann
        then resolve ann_marks tl_ins (update ins matched_anns) modul
        else resolve tl_anns instrs ((hd_ann, None) :: matched_anns) modul)
    | Bug_end (line, col) ->
      (match instrs with
      | [] ->
        let updated_matched_anns = apply_hd_bug hd_ann matched_anns modul in
        resolve tl_anns instrs updated_matched_anns modul
      | hd_ins :: tl_ins ->
        if less_than hd_ins hd_ann
        then resolve ann_marks tl_ins (update hd_ins matched_anns) modul
        else (
          let updated_matched_anns = apply_hd_bug hd_ann matched_anns modul in
          resolve tl_anns instrs updated_matched_anns modul))
    | Safe_end (line, col) ->
      (match instrs with
      | [] ->
        let updated_matched_anns = apply_hd_safe hd_ann matched_anns modul in
        resolve tl_anns instrs updated_matched_anns modul
      | hd_ins :: tl_ins ->
        if less_than hd_ins hd_ann
        then resolve ann_marks tl_ins (update hd_ins matched_anns) modul
        else (
          let updated_matched_anns = apply_hd_safe hd_ann matched_anns modul in
          resolve tl_anns instrs updated_matched_anns modul))
    | Skip -> resolve tl_anns instrs matched_anns modul)
;;

let instrument_bug_annotation ann_marks source_name (modul : LL.llmodule)
    : unit
  =
  (* TODO: fill code here.
     See module llsimp.ml, function elim_instr_intrinsic_lifetime ...
     for how to manipulating LLVM bitcode *)
  let _ =
    debug2 ~ruler:`Long "Uninstrumented: " (LL.string_of_llmodule modul) in
  let finstr =
    Some
      (fun acc instr ->
        let pos_op = LS.position_of_instr instr in
        match pos_op with
        | None -> acc
        | Some pos ->
          if not (String.equal pos.pos_file_name source_name)
          then acc
          else (
            match acc with
            | [] -> make_tagged_ins pos 1 instr :: acc
            | hd :: tl -> make_tagged_ins pos (hd.tag + 1) instr :: acc)) in
  let tagged_ins = deep_fold_module ~finstr [] modul in
  let compare ins1 ins2 =
    let p1 = ins1.pos.pos_line_end, ins1.pos.pos_col_end in
    let p2 = ins2.pos.pos_line_end, ins2.pos.pos_col_end in
    if Poly.(p1 > p2) then 1 else if Poly.(p1 < p2) then -1 else 0 in
  let sorted_ins = List.stable_sort ~compare tagged_ins in
  let _ = resolve ann_marks sorted_ins [] modul in
  (* Code to debug instructions *)
  (* let _ = debug ~ruler:`Short "Sorted_ins" in *)
  (*let tagnew = deep_fold_module ~finstr [] modul in *)
  (*let _ = *)
  (*  List.iter tagnew ~f:(fun ins -> *)
  (*      let ins_str = *)
  (*        match ins.ins with *)
  (*        | Instr inx -> LL.string_of_llvalue inx in *)
  (*      let cover = *)
  (*        match Hashtbl.find coverage ins.ins with *)
  (*        | None -> "None" *)
  (*        | Some (lc_start, lc_end) -> *)
  (*          (pr_int lc_start.line ^ " ") *)
  (*          ^ (pr_int lc_start.col ^ " ") *)
  (*          ^ (pr_int lc_end.line ^ " ") *)
  (*          ^ pr_int lc_end.col in *)
  (*      let llins = llvalue_of_instr ins.ins in *)
  (*            let args = *)
  (*        let num_args = LL.num_operands llins in *)
  (*        let rec fold_oprs acc idx = *)
  (*          if idx >= num_args *)
  (*          then acc ^ "\n" *)
  (*          else ( *)
  (*            let opr = LL.string_of_llvalue (LL.operand llins idx in) *)
  (*            fold_oprs (acc ^ "." ^ opr ) (idx + 1)) in *)
  (*        fold_oprs "" 0 in *)
  (*      let instype = LL.string_of_lltype (LL.type_of llins) in *)
  (*      debug2 "Ins: " (ins_str ^ "..Type: " ^ instype ^ ".." ^ cover)) in *)
  (*  Code to debug coverage *)
  (* let strhash = Hashtbl.fold coverage ~init:"" ~f:(fun ~key ~data acc -> *)
  (*   let findx = Hashtbl.find coverage key in *)
  (*   let findstr = match findx with *)
  (*   | None -> "None" *)
  (*   | Some _ -> "Some" in *)
  (*   acc ^ (LL.string_of_llvalue (llvalue_of_instr key)) ^ " " ^ *)
  (*     (match data with *)
  (*     | (st, fn) -> *)
  (*       pr_int st.line ^ " " ^ pr_int st.col ^ " " ^ *)
  (*       pr_int fn.line ^ " " ^ pr_int fn.col *)
  (*   ) ^ " " ^ findstr ^ "\n" *)
  (* ) in *)
  (* let _ = debug2 ~ruler:`Long "Hashtbl" strhash in *)
   debug2 ~ruler:`Long "Instrumented" (LL.string_of_llmodule modul)
;;

(*we need source_name to ignore instructions with location outside the source file *)
let instrument_bitcode ann_marks source_name (modul : LL.llmodule) : unit =
  instrument_bug_annotation ann_marks source_name modul
;;
