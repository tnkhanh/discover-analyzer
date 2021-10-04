(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore
open Llir

module LL = Llvm
module LO = Llvm.Opcode
module LD = Llvm_debuginfo
module LS = Llsrc
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcfg


let instrument_bug_annotation (modul: LL.llmodule) : unit =
  (* TODO: fill code here.
     See module llsimp.ml, function elim_instr_intrinsic_lifetime ...
     for how to manipulating LLVM bitcode *)
  let _ = print "INSTRUMENT BUG ANNOTATION" in
  ()

let extract_annotations (filename: string) = 
  let _ = print_endline ("File:.........."^filename) in
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
            (Str.regexp "/\\*[ \t]*{[ \t]*Bug:\\([^*]*\\)\\*/") line col_number
          with
          | Not_found -> -1 in

        if match_pos = -1 then old_list
        else
          find_annot (Str.match_end ())
            ((line_number, (Str.match_end ()) + 1, Str.matched_group 1 line)::old_list) in

      let new_list = find_annot 0 annot_list in
      read_line (line_number+1) new_list
    with
    | End_of_file -> annot_list in
  read_line 1 []

let instrument_bitcode filename (modul: LL.llmodule) : unit =
  let annotations = extract_annotations 
    ((String.sub filename ~pos:0 ~len:(String.length filename - 11))^"c") in
  let _ = print_endline "Annotations: " in
  let _ = 
    List.iter 
      ~f:(fun (x, y, s) -> print_endline ((string_of_int x)^" "^(string_of_int y)^" "^s))
      annotations in
 
  instrument_bug_annotation modul
