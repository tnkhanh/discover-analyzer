(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(*******************************************************************
 * Extending library String with new functionalities
 *******************************************************************)

module String = struct
  (*--------------------------------
   * New functions handling string
   *-------------------------------*)

  let not_empty (str : string) : bool = not (String.is_empty str)

  let is_infix ~(infix : string) (str : string) : bool =
    let idxs = String.substr_index_all ~may_overlap:false ~pattern:infix str in
    let len, sublen = String.length str, String.length infix in
    List.exists ~f:(fun idx -> idx > 0 && idx < len - sublen) idxs
  ;;

  let strip_newline (str : string) : string =
    String.strip ~drop:(fun c -> c == '\n') str
  ;;

  let prefix_if_not_empty (s : string) ~(prefix : string) : string =
    if String.is_empty s then s else prefix ^ s
  ;;

  let suffix_if_not_empty (s : string) ~(suffix : string) : string =
    if String.is_empty s then s else s ^ suffix
  ;;

  let surround_if_not_empty (s : string) ~prefix ~suffix : string =
    if String.is_empty s then s else prefix ^ s ^ suffix
  ;;

  let replace_if_empty (s : string) ~(replacer : string) : string =
    if String.is_empty s then replacer else s
  ;;

  (*-----------------------
   * Formatting string
   *----------------------*)

  let count_indent (str : string) : int =
    let str = String.lstrip ~drop:(fun c -> c == '\n') str in
    let index = String.lfindi ~f:(fun _ c -> c != ' ') str in
    match index with
    | None -> 0
    | Some i -> i
  ;;

  let mk_indent (indent : int) : string =
    let rec mk_whitespace (i : int) : string =
      if i <= 0 then "" else " " ^ mk_whitespace (i - 1) in
    mk_whitespace indent
  ;;

  (** format a message by insert an indentation to each line *)

  let indent_line ?(skipfirst = false) (indent : int) (msg : string) : string =
    let sindent = mk_indent indent in
    msg
    |> String.split ~on:'\n'
    |> List.mapi ~f:(fun i s -> if i = 0 && skipfirst then s else sindent ^ s)
    |> String.concat ~sep:"\n"
  ;;

  (** high-order insert an indentation to each line of a message *)
  let hindent_line ?(skipfirst = false) (i : int) (f : 'a -> string) (v : 'a)
      : string
    =
    indent_line ~skipfirst i (f v)
  ;;

  (** auto-insert indentation to align_line with the prefix string *)
  let align_line (prefix : string) (msg : string) : string =
    let prefix = String.strip ~drop:(fun c -> c == '\n') prefix in
    let indent = String.length prefix in
    let skipfirst = not (String.is_suffix ~suffix:"\n" prefix) in
    prefix ^ indent_line ~skipfirst indent msg
  ;;

  (** high-order auto-insert indentation to align_line with the prefix string *)
  let halign_line prefix (f : 'a -> string) (v : 'a) : string =
    align_line prefix (f v)
  ;;

  (** insert a prefix to each line of a string *)
  let prefix_line ~(prefix : string) (msg : string) : string =
    msg
    |> String.split_lines
    |> List.map ~f:(fun s -> prefix ^ s)
    |> String.concat ~sep:"\n"
  ;;

  (*-------------------------------------
   * Include the existing String library
   *------------------------------------*)

  include String
end
