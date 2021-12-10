(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir
open Llannot
module LL = Llvm
module LO = Llvm.Opcode
module LD = Llvm_debuginfo
module LS = Lldebug
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph
module BG = Bug

exception AnnotFormat of string

type tagged_instr =
  { tagx_pos : position;
    tagx_tag : int;
    tagx_instr : instr
  }

let mk_tagged_instr (pos : position) (tag : int) instr =
  { tagx_pos = pos; tagx_tag = tag; tagx_instr = instr }
;;

let compare_position (pos1 : annot_position) (pos2 : annot_position) : int =
  let line1, col1 = pos1.apos_line, pos1.apos_col in
  let line2, col2 = pos2.apos_line, pos2.apos_col in
  Poly.compare (line1, col1) (line2, col2)
;;

let max_position (pos1 : annot_position) (pos2 : annot_position)
    : annot_position
  =
  if compare_position pos1 pos2 = 1 then pos1 else pos2
;;

let min_position (pos1 : annot_position) (pos2 : annot_position)
    : annot_position
  =
  if compare_position pos1 pos2 = 1 then pos2 else pos1
;;

let is_instr_before_annotation (instr : tagged_instr) (ann : bug_annot) : bool =
  let instr_pos = instr.tagx_pos in
  let instr_line = instr_pos.pos_line_end in
  let instr_col = instr_pos.pos_col_end in
  let annot_pos = get_annot_position ann in
  let annot_line = annot_pos.apos_line in
  let annot_col = annot_pos.apos_col in
  Poly.((instr_line, instr_col) < (annot_line, annot_col))
;;

let coverage = Hashtbl.create (module InstrKey)

let rec get_coverage instr =
  match Hashtbl.find coverage instr with
  | None ->
    let pos_op = LS.position_of_instr instr in
    let oprc = num_operands instr in
    let cover =
      let init =
        match pos_op with
        | None ->
          ( { apos_line = max_int; apos_col = max_int },
            { apos_line = 0; apos_col = 0 } )
        | Some pos ->
          ( mk_annot_position pos.pos_line_start pos.pos_col_start,
            mk_annot_position pos.pos_line_end pos.pos_col_end ) in
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
              let new_start = min_position opr_cov_start acc_start in
              let new_end = max_position opr_cov_end acc_end in
              fold_opr (new_start, new_end) (idx + 1))
          | _ -> fold_opr acc (idx + 1)) in
      fold_opr init 0 in
    let _ = Hashtbl.add coverage ~key:instr ~data:cover in
    cover
  | Some cover -> cover
;;

let extract_bug_annotations (input_file : string) : bug_annots =
  let inx = In_channel.create input_file in
  let lexbuf = Lexing.from_channel inx in
  let rev_mark_list =
    try Llannot_parser.prog Llannot_lexer.read lexbuf
    with Llannot_parser.Error ->
      let _ =
        debug ("Parsing failed. Annotations ignored in file: " ^ input_file)
      in
      [] in
  (*print all marks*)
  let _ = if !print_instrumented_prog then debug ~ruler:`Short "Annotations" in
  let _ =
    if !print_instrumented_prog
    then List.iter ~f:debug (List.rev (List.map rev_mark_list ~f:pr_bug_annot))
  in
  List.rev rev_mark_list
;;

let generate_instrumented_func_name anntyp (bug : BG.bug_type) ins_type =
  let base_name =
    match anntyp with
    | Bug ->
      (match bug with
      | MemoryLeak _ -> "__assert_memory_leak"
      | NullPointerDeref _ -> "__assert_null_pointer_deref"
      | BufferOverflow _ -> "__assert_buffer_overflow"
      | IntegerOverflow _ -> "__assert_bug_integer_overflow"
      | IntegerUnderflow _ -> "__assert_integer_underflow"
      | DivisionByZero _ -> "__assert_division_by_zero"
      | _ ->
        let _ =
          hwarning
            "generate_instrumented_func_name: bug type not yet supported: "
            BG.pr_bug_type bug in
        "__assert_unsupported_instrumented_func")
    | Safe ->
      (match bug with
      | MemoryLeak _ -> "__refute_memory_leak"
      | NullPointerDeref _ -> "__refute_null_pointer_deref"
      | BufferOverflow _ -> "__refute_buffer_overflow"
      | IntegerOverflow _ -> "__refute_bug_integer_overflow"
      | IntegerUnderflow _ -> "__refute_integer_underflow"
      | DivisionByZero _ -> "__refute_division_by_zero"
      | _ ->
        let _ =
          hwarning
            "generate_instrumented_func_name: bug type not yet supported: "
            BG.pr_bug_type bug in
        "__refute_unsupported_instrumented_func") in
  let tail = LL.string_of_lltype ins_type in
  base_name ^ "_" ^ tail
;;

let generate_instrumented_func_args (bug : BG.bug_type) (instr : value)
    : value array
  =
  match bug with
  | _ -> [| instr |]
;;

let apply_annotation
    (anntyp : annot_type)
    (instr : tagged_instr)
    (bugs : BG.bug_types)
    (modul : llmodule)
  =
  match instr.tagx_instr with
  | Instr inx ->
    (* if instr is store and first operand is trunc *)
    let actual_ins =
      if is_instr_store instr.tagx_instr
      then (
        let first_opr = operand instr.tagx_instr 0 in
        match LL.instr_opcode first_opr with
        | LO.Trunc -> first_opr
        | _ -> inx)
      else inx in
    let insert_pos = LL.instr_succ actual_ins in
    let llctx = LL.module_context modul in
    let builder = LL.builder_at llctx insert_pos in
    let ins_type = LL.type_of actual_ins in
    List.iter bugs ~f:(fun bug ->
        let func_name = generate_instrumented_func_name anntyp bug ins_type in
        let assert_func_opt = LL.lookup_function func_name modul in
        let assert_func =
          match assert_func_opt with
          | None ->
            let func_type =
              LL.function_type (LL.void_type llctx) [| ins_type |] in
            let func = LL.declare_function func_name func_type modul in
            func
          | Some func -> func in
        let args = generate_instrumented_func_args bug actual_ins in
        ignore (LL.build_call assert_func args "" builder))
;;

let rec update (instr : tagged_instr) anns =
  match anns with
  | [] -> []
  | hd_match :: tl ->
    (match hd_match with
    | ann, ins_op ->
      (match ins_op with
      | None -> (ann, Some instr) :: update instr tl
      | Some old_ins ->
        let choose_ins =
          let old_start, old_end = get_coverage old_ins.tagx_instr in
          let cstart, cend = get_coverage instr.tagx_instr in
          let start_comp = compare_position old_start cstart in
          let end_comp = compare_position old_end cend in
          if start_comp < 0
             || (start_comp = 0 && end_comp > 0)
             || (start_comp = 0 && end_comp = 0
                && old_ins.tagx_tag < instr.tagx_tag)
          then old_ins
          else instr in
        (ann, Some choose_ins) :: update instr tl))
;;

let apply_bug_annotation
    (ann_type : annot_type)
    (end_ann : bug_annot)
    (matched_anns : (bug_annot * tagged_instr option) list)
    (modul : LL.llmodule)
  =
  let pos = get_annot_position end_ann in
  match matched_anns with
  | [] ->
    herror "Bug annotation: ending without start at " pr_annot_position pos
  | (ann, ins_op) :: other_matched_anns ->
    (match ann with
    | Bug_start (pos, bugs) ->
      (match ins_op with
      | None ->
        herror "Bug annotation: no instr for annot at " pr_annot_position pos
      | Some instr ->
        let _ = apply_annotation ann_type instr bugs modul in
        other_matched_anns)
    | Bug_end _ | Safe_start _ | Safe_end _ | Skip ->
      herror "Bug annotation: unmatch ending at " pr_annot_position pos)
;;

let rec resolve
    (annots : bug_annot list)
    (instrs : tagged_instr list)
    matched_anns
    modul
  =
  match annots with
  | [] -> ()
  | hd_ann :: tl_anns ->
    (match hd_ann with
    | Bug_start (pos, bugs) | Safe_start (pos, bugs) ->
      (match instrs with
      | [] ->
        errorf
          "Bug annotation: no matching instruction for annotation at: %d:%d"
          pos.apos_line pos.apos_col
      | instr :: tl_ins ->
        if is_instr_before_annotation instr hd_ann
        then resolve annots tl_ins (update instr matched_anns) modul
        else resolve tl_anns instrs ((hd_ann, None) :: matched_anns) modul)
    | Bug_end pos ->
      (match instrs with
      | [] ->
        let updated_matched_anns =
          apply_bug_annotation Bug hd_ann matched_anns modul in
        resolve tl_anns instrs updated_matched_anns modul
      | hd_ins :: tl_ins ->
        if is_instr_before_annotation hd_ins hd_ann
        then resolve annots tl_ins (update hd_ins matched_anns) modul
        else (
          let updated_matched_anns =
            apply_bug_annotation Bug hd_ann matched_anns modul in
          resolve tl_anns instrs updated_matched_anns modul))
    | Safe_end pos ->
      (match instrs with
      | [] ->
        let updated_matched_anns =
          apply_bug_annotation Safe hd_ann matched_anns modul in
        resolve tl_anns instrs updated_matched_anns modul
      | hd_ins :: tl_ins ->
        if is_instr_before_annotation hd_ins hd_ann
        then resolve annots tl_ins (update hd_ins matched_anns) modul
        else (
          let updated_matched_anns =
            apply_bug_annotation Safe hd_ann matched_anns modul in
          resolve tl_anns instrs updated_matched_anns modul))
    | Skip -> resolve tl_anns instrs matched_anns modul)
;;

let instrument_bug_annotation annots source_name (modul : LL.llmodule) : unit =
  let _ =
    if !print_instrumented_prog
    then hdebug ~ruler:`Long "Uninstrumented: " LL.string_of_llmodule modul
  in
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
            | [] -> mk_tagged_instr pos 1 instr :: acc
            | hd :: tl -> mk_tagged_instr pos (hd.tagx_tag + 1) instr :: acc))
  in
  let tagged_instr = fold_struct_module ~finstr [] modul in
  let compare ins1 ins2 =
    let p1 = ins1.tagx_pos.pos_line_end, ins1.tagx_pos.pos_col_end in
    let p2 = ins2.tagx_pos.pos_line_end, ins2.tagx_pos.pos_col_end in
    if Poly.(p1 > p2) then 1 else if Poly.(p1 < p2) then -1 else 0 in
  let sorted_ins = List.stable_sort ~compare tagged_instr in
  let _ = resolve annots sorted_ins [] modul in
  if !print_instrumented_prog
  then hdebug ~ruler:`Long "Instrumented" LL.string_of_llmodule modul
;;

(*we need source_name to ignore instructions with location outside the source file *)
let instrument_bitcode annots source_name (modul : LL.llmodule) : unit =
  instrument_bug_annotation annots source_name modul
;;
