(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir
open Llannot
module LL = Llvm
module LO = Llvm.Opcode
module LD = Llvm_debuginfo
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph
module BG = Bug

exception AnnotFormat of string

let assert_counter = ref 0

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
    let pos_op = position_of_instr instr in
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

let generate_instrumented_func_name
    ann_class
    anntyp
    (bug : BG.bug_type)
    ins_type
  =
  let prefix =
    if String.equal anntyp "Bug"
    then "__assert_" ^ ann_class ^ "_"
    else if String.equal anntyp "Safe"
    then "__refute_" ^ ann_class ^ "_"
    else "__wonder_" ^ ann_class ^ "_" in
  let bug_name = BG.pr_bug_type_lowercase bug in
  let tail = LL.string_of_lltype ins_type in
  prefix ^ bug_name ^ "_" ^ tail
;;

let generate_instrumented_func_args (instr : value) (llctx : LL.llcontext)
    : value array
  =
  let counter_arg = LL.const_int (LL.i32_type llctx) !assert_counter in
  [| counter_arg; instr |]
;;

let apply_annotation
    (anntyp : string)
    (instrs : tagged_instr list)
    (bug : BG.bug_type)
    (modul : bitcode_module)
  =
  List.iter
    ~f:(fun instr ->
      match instr.tagx_instr with
      | Instr inx ->
        let actual_ins = inx in
        let _ =
          if !print_instrumented_prog && !mode_deep_debug
          then debug ("Actual Apply: " ^ LL.string_of_llvalue actual_ins) in
        let insert_pos = LL.instr_succ actual_ins in
        let llctx = LL.module_context modul in
        let builder = LL.builder_at llctx insert_pos in
        let ins_type = LL.type_of actual_ins in
        let func_name =
          generate_instrumented_func_name "ins" anntyp bug ins_type in
        let assert_func_opt = LL.lookup_function func_name modul in
        let assert_func =
          match assert_func_opt with
          | None ->
            let func_type =
              LL.function_type (LL.void_type llctx)
                [| LL.i32_type llctx; ins_type |] in
            let func = LL.declare_function func_name func_type modul in
            func
          | Some func -> func in
        let args = generate_instrumented_func_args actual_ins llctx in
        ignore (LL.build_call assert_func args "" builder))
    instrs
;;

let update (instr : tagged_instr) matched_anns =
  List.map
    ~f:(fun (ann, instrs) ->
      match instrs with
      | [] -> ann, [ instr ]
      | old_ins :: _ as all_ins ->
        let choose_ins =
          let old_start, old_end = get_coverage old_ins.tagx_instr in
          let cstart, cend = get_coverage instr.tagx_instr in
          let start_comp = compare_position old_start cstart in
          let end_comp = compare_position old_end cend in
          if start_comp = 0 && end_comp = 0
          then instr :: all_ins
          else if start_comp < 0 || (start_comp = 0 && end_comp > 0)
          then all_ins
          else [ instr ] in
        ann, choose_ins)
    matched_anns
;;

let apply_bug_annotation
    (end_ann : bug_annot)
    (matched_anns : (bug_annot * tagged_instr list) list)
    (modul : LL.llmodule)
  =
  let pos = get_annot_position end_ann in
  match matched_anns with
  | [] ->
    errorp "Bug annotation: ending without start at " pr_annot_position pos
  | (ann, instrs) :: other_matched_anns ->
    let instrc = List.length instrs in
    if instrc = 0
    then errorp "Bug annotation: no instr for annot at " pr_annot_position pos
    else (
      (* if instrc = 1 then *)
      let ann_type = get_annot_type end_ann in
      if String.equal (get_annot_type ann) ann_type
      then (
        let bugs = get_annot_bugs ann in
        let _ =
          List.iter
            ~f:(fun bug ->
              let _ = assert_counter := !assert_counter + 1 in
              apply_annotation ann_type instrs bug modul)
            bugs in
        other_matched_anns)
      else errorp "Bug annotation: unmatched ending at " pr_annot_position pos)
;;

let apply_line_annotation
    (ann : bug_annot)
    (instr : instr)
    (modul : bitcode_module)
  =
  let ins = llvalue_of_instr instr in
  let ins_type = LL.type_of ins in
  let llctx = LL.module_context modul in
  match ann with
  | Line (line, ann_type, bugs) ->
    List.iter
      ~f:(fun bug ->
        let builder = LL.builder_before llctx ins in
        let func_name =
          generate_instrumented_func_name "line" ann_type bug ins_type in
        let assert_func_opt = LL.lookup_function func_name modul in
        let int_type = LL.i32_type llctx in
        let assert_func =
          match assert_func_opt with
          | None ->
            let func_type =
              LL.function_type (LL.void_type llctx) [| int_type |] in
            let func = LL.declare_function func_name func_type modul in
            func
          | Some func -> func in
        let args = [| LL.const_int int_type line |] in
        ignore (LL.build_call assert_func args "" builder))
      bugs
  | _ -> error "Wrong annotation class, expected Line"
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
    | Start (pos, atype, bugs) ->
      (match instrs with
      | [] ->
        errorf
          "Bug annotation: no matching instruction for annotation at: %d:%d"
          pos.apos_line pos.apos_col
      | instr :: tl_ins ->
        if is_instr_before_annotation instr hd_ann
        then resolve annots tl_ins (update instr matched_anns) modul
        else resolve tl_anns instrs ((hd_ann, []) :: matched_anns) modul)
    | End (pos, atype) ->
      (match instrs with
      | [] ->
        let updated_matched_anns =
          apply_bug_annotation hd_ann matched_anns modul in
        resolve tl_anns instrs updated_matched_anns modul
      | hd_ins :: tl_ins ->
        if is_instr_before_annotation hd_ins hd_ann
        then resolve annots tl_ins (update hd_ins matched_anns) modul
        else (
          let updated_matched_anns =
            apply_bug_annotation hd_ann matched_anns modul in
          resolve tl_anns instrs updated_matched_anns modul))
    | Line (line, atype, bugs) ->
      (match instrs with
      | [] -> errorp "No match for annotation at line: " pr_int line
      | hd_ins :: tl_ins ->
        if is_instr_before_annotation hd_ins hd_ann
        then resolve annots tl_ins matched_anns modul
        else (
          let _ =
            if hd_ins.tagx_pos.pos_line_start = line + 1
            then apply_line_annotation hd_ann hd_ins.tagx_instr modul in
          resolve tl_anns tl_ins matched_anns modul))
    | Skip -> resolve tl_anns instrs matched_anns modul)
;;

let bug_annotation annots source_name (modul : LL.llmodule) : unit =
  let _ =
    if !print_instrumented_prog && !mode_deep_debug
    then debugp ~ruler:`Long "Uninstrumented: " LL.string_of_llmodule modul
  in
  let finstr =
    Some
      (fun acc instr ->
        let _ =
          if !print_instrumented_prog && !mode_deep_debug
          then (
            let ins = llvalue_of_instr instr in
            let _ = print ("Ins: " ^ LL.string_of_llvalue ins) in
            let oprs = operands instr in
            List.iter
              ~f:(fun opr ->
                print
                  ("Opr: " ^ LL.string_of_llvalue opr ^ " ValueKind: "
                  ^ pr_valuekind (LL.classify_value opr)))
              oprs) in
        if is_instr_store instr
        then (
          let _ =
            if !print_instrumented_prog && !mode_deep_debug
            then debug ("Store: " ^ LL.string_of_llvalue (operand instr 0))
          in
          acc)
        else (
          let pos_op = position_of_instr instr in
          match pos_op with
          | None -> acc
          | Some pos ->
            if not (String.equal pos.pos_file_name source_name)
            then acc
            else (
              match acc with
              | [] -> mk_tagged_instr pos 1 instr :: acc
              | hd :: tl -> mk_tagged_instr pos (hd.tagx_tag + 1) instr :: acc)))
  in
  let tagged_instr = visit_fold_module ~finstr [] modul in
  let _ =
    if !print_instrumented_prog && !mode_deep_debug
    then
      List.iter
        ~f:(fun tagged ->
          let ins = tagged.tagx_instr in
          debug ("Instrs: " ^ LL.string_of_llvalue (llvalue_of_instr ins)))
        tagged_instr in
  let compare ins1 ins2 =
    let p1 = ins1.tagx_pos.pos_line_end, ins1.tagx_pos.pos_col_end in
    let p2 = ins2.tagx_pos.pos_line_end, ins2.tagx_pos.pos_col_end in
    if Poly.(p1 > p2) then 1 else if Poly.(p1 < p2) then -1 else 0 in
  let sorted_ins = List.stable_sort ~compare tagged_instr in
  resolve annots sorted_ins [] modul
;;

(*we need source_name to ignore instructions with location outside the source file *)
let instrument_bitcode annots source_name (modul : LL.llmodule) : unit =
  bug_annotation annots source_name modul
;;
