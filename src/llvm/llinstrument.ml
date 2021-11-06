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
  { pos : position
  ; tag : int
  ; ins : instr
  }

let make_tagged_ins pos_ tag_ ins_ = { pos = pos_; tag = tag_; ins = ins_ }

let less_than ins ann =
  if Poly.((ins.pos.pos_line_end, ins.pos.pos_col_end) < Ann.pos_of_ann ann)
  then true
  else false
;;

let extract_ann_marks (filename : string) =
  (*  let _ = print_endline ("File:.........."^filename) in
  let inchan =
    try open_in filename
    with e -> error ("Unable to open file: " ^ filename) in
  let rec read_line line_number annot_list =
    try
      let line = input_line inchan in
      let rec find_annot col_number old_list =
        let match_pos =
          try
          (*NOTE: no asterisk allowed in Bug string*)
            Str.search_forward
            (Str.regexp "/\\*{\\(Bug\\):\\([^*]*\\)\\*/") line col_number
          with Not_found -> -1 in
        if match_pos = -1 then old_list
        else
          find_annot (Str.match_end ())
            (((line_number, (Str.match_end ()) + 1), Str.matched_group 2 line)::old_list) in
      let new_list = find_annot 0 annot_list in
      read_line (line_number+1) new_list
    with End_of_file -> let _ = close_in inchan in annot_list in
  read_line 1 [] *)
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let rev_mark_list =
    try Annparser.prog Annlexer.read lexbuf with
    | Annparser.Error ->
      let _ =
        hdebug "Parsing failed. Annotations ignored in file: " pr_str filename
      in
      [] in
  (*print all marks*)
  let _ =
    List.iter
      (List.rev (List.map rev_mark_list ~f:Ann.str_of_mark))
      ~f:print_endline in
  List.rev rev_mark_list
;;

let get_func_name anntyp (bug : Ann.bug_group) ins =
  match anntyp with
  | Bug ->
    (match bug with
    | MemoryLeak -> "__assert_memory_leak"
    | NullPointerDeref -> "__assert_null_pointer_deref"
    | BufferOverflow -> "__assert_buffer_overflow"
    | IntegerOverflow ->
      if LL.integer_bitwidth (LL.type_of ins) = 64
      then "__assert_bug_integer_overflow_i64"
      else "__assert_bug_integer_overflow_i32"
    | IntegerUnderflow -> "__assert_integer_underflow"
    | DivisionByZero -> "__assert_division_by_zero"
    | NewType t -> "__assert_unnamed_bug")
  | Safe ->
    (match bug with
    | MemoryLeak -> "__refute_memory_leak"
    | NullPointerDeref -> "__refute_null_pointer_deref"
    | BufferOverflow -> "__refute_buffer_overflow"
    | IntegerOverflow ->
      if LL.integer_bitwidth (LL.type_of ins) = 64
      then "__refute_bug_integer_overflow_i64"
      else "__refute_bug_integer_overflow_i32"
    | IntegerUnderflow -> "__refute_integer_underflow"
    | DivisionByZero -> "__refute_division_by_zero"
    | NewType t -> "__refute_unnamed_bug")
;;

let apply_annotation anntyp instr bugs modul =
  match instr.ins with
  | Instr inx ->
    let insert_pos = LL.instr_succ inx in
    let builder = LL.builder_at (LL.module_context modul) insert_pos in
    List.iter bugs ~f:(fun bug ->
        let assert_func_opt =
          LL.lookup_function (get_func_name anntyp bug inx) modul in
        match assert_func_opt with
        | None -> ()
        | Some assert_func ->
          let assert_ins =
            LL.build_call assert_func (Array.create ~len:1 inx) "" builder in
          ())
;;

let rec update ins anns =
  match anns with
  | [] -> []
  | hd_match :: tl ->
    (match hd_match with
    | ann, ins_op ->
      (match ins_op with
      | None -> (ann, Some ins) :: update ins tl
      | Some old_ins ->
        (ann, Some (if old_ins.tag < ins.tag then ins else old_ins))
        :: update ins tl))
;;

let apply_hd_bug
    end_ann
    (matched_anns : (Ann.mark * instr_with_tag option) list)
    modul
  =
  match matched_anns with
  | [] ->
    raise
      (AnnotFormat ("Error: Bug_end without start at " ^ Ann.pr_pos_ann end_ann))
  | (ann, ins_op) :: tl ->
    (match ann with
    | Bug_start ((line, col), bugs) ->
      (match ins_op with
      | None ->
        raise
          (AnnotFormat ("Error: No ins for annot at " ^ Ann.pr_pos_ann end_ann))
      | Some ins ->
        let _ = apply_annotation Bug ins bugs modul in
        tl)
    | Bug_end _ | Safe_start _ | Safe_end _ | Skip ->
      raise
        (AnnotFormat
           ("Error: no Bug_start matching Bug_end at " ^ Ann.pr_pos_ann end_ann)))
;;

let apply_hd_safe
    end_ann
    (matched_anns : (Ann.mark * instr_with_tag option) list)
    modul
  =
  match matched_anns with
  | [] ->
    raise
      (AnnotFormat ("Error: Safe_end without start at " ^ Ann.pr_pos_ann end_ann))
  | (ann, ins_op) :: tl ->
    (match ann with
    | Safe_start ((line, col), bugs) ->
      (match ins_op with
      | None ->
        raise
          (AnnotFormat ("Error: No ins for annot at " ^ Ann.pr_pos_ann end_ann))
      | Some ins ->
        let _ = apply_annotation Safe ins bugs modul in
        tl)
    | Bug_start _ | Bug_end _ | Safe_end _ | Skip ->
      raise
        (AnnotFormat
           ("Error: no Safe_start matching Safe_end at" ^ Ann.pr_pos_ann end_ann)))
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

let instrument_bug_annotation ann_marks source_name (modul : LL.llmodule) : unit
  =
  (* TODO: fill code here.
     See module llsimp.ml, function elim_instr_intrinsic_lifetime ...
     for how to manipulating LLVM bitcode *)

  (*  let _ = print "INSTRUMENT BUG ANNOTATION" in

  let sorted_anns = List.rev annotations in
  let _ =
    List.iter ~f:(fun ((line, col), ann) ->
      print_endline (ann^"----------"^(pr_int line)^"------------"^(pr_int col))
    ) sorted_anns in *)

  (* map each instr to (line, col) * tag * instr *)
  let _ = hprint "======== Source file: " pr_str source_name in
  let _ =
    print_endline
      ("MODULE  =============================\n"
      ^ LL.string_of_llmodule modul
      ^ " =====================\n") in
  let finstr =
    Some
      (fun acc instr ->
        let pos_op = LS.position_of_instr instr in
        match pos_op with
        | None ->
          acc
          (* let pop = mk_position "Hi!" (-1) (-1) (-1) (-1) in
        let ins = make_tagged_ins pop (-1) instr in
        ins::acc ) *)
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
  let _ = print_endline "Tags  =============================\n" in
  let sorted_ins = List.stable_sort ~compare tagged_ins in
  let llctx = LL.global_context () in
  let _ =
    List.iter
      ~f:(fun tin ->
        match tin.ins with
        | Instr inx ->
          let dbg = LL.metadata inx (LL.mdkind_id llctx "dbg") in
          let dbg_str =
            match dbg with
            | None -> "None.."
            | Some d -> LL.string_of_llvalue d in
          print_endline
            ((*        "Tag: " ^ (pr_int tin.tag) ^ "\n" ^
                     "Debug: " ^ dbg_str ^ "\n" ^
        "Instr: " ^ (LL.string_of_llvalue inx) ^ " +++ " ^ (pr_int tin.pos.pos_line_end) ^ " .. "
        ^ (pr_int tin.pos.pos_col_end)
        ^ " .. file: " ^ tin.pos.pos_file_name  *)
             LL.string_of_llvalue
               inx)
        (*      let pos = LS.position_of_instr instr in
        match pos with
        | None -> ()
        | Some p -> print_endline ((LL.string_of_llvalue inx)^"++++ "^
                         (pr_int line) ^ "__" ^
                         (pr_int col)) *))
      (List.rev tagged_ins)
    (*sorted_ins*) in
  let _ = print_endline "SORTED_INS===" in
  let _ =
    List.iter tagged_ins ~f:(fun ins ->
        match ins.ins with
        | Instr inx -> print_endline (LL.string_of_llvalue inx)) in
  let _ = print_endline "===SORTED_INS" in
  let _ = resolve ann_marks sorted_ins [] modul in
  print_endline
    ("INSTRUMENTED ***********************\n"
    ^ LL.string_of_llmodule modul
    ^ "***********************")
;;

(*we need source_name to ignore instructions with location outside the source file *)
let instrument_bitcode ann_marks source_name (modul : LL.llmodule) : unit =
  instrument_bug_annotation ann_marks source_name modul
;;
