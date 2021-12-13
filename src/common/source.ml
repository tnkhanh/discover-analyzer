(********************************************************************
 * this file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore

let pr_file_excerpt
    filename
    (lstart : int)
    (lend : int)
    (cstart : int)
    (cend : int)
  =
  (* let _ = print_endline ("File: " ^ filename) in *)
  let file_lines = In_channel.read_lines filename in
  let num_lines = List.length file_lines in
  let lstart = if lstart < 3 then 3 else lstart in
  let lend = if lend > num_lines - 2 then num_lines - 2 else lend in
  let rec pr_excerpt lines lcur acc =
    match lines with
    | [] -> List.rev acc
    | line :: nlines ->
      let nl = lcur + 1 in
      if lcur < lstart - 1 || lcur >= lend
      then (
        let marked_line = Printf.sprintf "%6d" nl ^ "|  " ^ line ^ "\n" in
        pr_excerpt nlines (lcur + 1) (marked_line :: acc))
      else (
        let marked_line = Printf.sprintf "%6d" nl ^ "|> " ^ line ^ "\n" in
        let marked_col =
          if lcur = lstart - 1
          then (
            let nc = if cstart > 2 then cstart - 2 else 0 in
            "      |> " ^ String.make nc ' ' ^ "^^^\n")
          else if lcur = lend - 1
          then (
            let nc = if cend > 2 then cend - 2 else 0 in
            "      |> " ^ String.make nc ' ' ^ "^^^\n")
          else "" in
        pr_excerpt nlines (lcur + 1) (marked_col :: marked_line :: acc)) in
  let excerpt_lines = List.slice file_lines (lstart - 3) (lend + 2) in
  let format_str = pr_excerpt excerpt_lines (lstart - 3) [] in
  String.rstrip (String.concat ~sep:"" format_str)
;;

let pr_file_position_and_excerpt (p : position) =
  let fname = p.pos_file_name in
  let lstart, lend = p.pos_line_start, p.pos_line_end in
  let cstart, cend = p.pos_col_start, p.pos_col_end in
  let line_column =
    if lstart = lend && cstart = cend
    then pr_int lstart ^ ":" ^ pr_int cstart
    else
      pr_int lstart ^ ":" ^ pr_int cstart ^ " ~> " ^ pr_int lend ^ ":"
      ^ pr_int cend in
  "File: " ^ fname ^ ", line/column position: " ^ line_column ^ "\n"
  ^ pr_file_excerpt fname lstart lend cstart cend
;;
