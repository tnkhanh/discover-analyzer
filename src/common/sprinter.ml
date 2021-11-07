(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(** String printer module *)

open Core
open Libdiscover

(*******************************************************************
 ** Utilites
 *******************************************************************)

let get_indent (msg : string) : int =
  let msg = String.lstrip ~drop:(fun ch -> ch == '\n') msg in
  if String.is_empty msg
  then 0
  else (
    try
      for i = 0 to String.length msg do
        if String.get msg i != ' ' then raise (EInt i)
      done;
      String.length msg
    with EInt res -> res)
;;

let string_of_indent (indent : int) : string =
  let rec mk_whitespace (i : int) : string =
    if i <= 0 then "" else " " ^ mk_whitespace (i - 1) in
  mk_whitespace indent
;;

(*******************************************************************
 ** Basic printer
 *******************************************************************)

let pr_id x = x
let string_of_bool : bool -> string = string_of_bool
let string_of_float : float -> string = string_of_float
let string_of_int (x : int) : string = string_of_int x
let string_of_int64 = Int64.to_string
let string_of_string (s : string) : string = s

(** format a message by insert an indentation to each line *)

let indent_line ?(skipfirst = false) (indent : int) (msg : string) : string =
  let sindent = string_of_indent indent in
  msg |> String.split ~on:'\n'
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
  let indent = String.length (String.strip_newline prefix) in
  let skipfirst = not (String.is_suffix ~suffix:"\n" prefix) in
  prefix ^ indent_line ~skipfirst indent msg
;;

(** high-order auto-insert indentation to align_line with the prefix string *)
let halign_line prefix (f : 'a -> string) (v : 'a) : string =
  align_line prefix (f v)
;;

(** insert a prefix to each line of a string *)
let prefix_line ~(prefix : string) (msg : string) : string =
  msg |> String.split_lines
  |> List.map ~f:(fun s -> prefix ^ s)
  |> String.concat ~sep:"\n"
;;

(*------------------
 * Print to string
 *-----------------*)

let sprintf = Printf.sprintf

let sprint_list
    ?(sep = ", ")
    ?(obrace = "[")
    ?(cbrace = "]")
    ?(indent = "")
    ?(extra = "")
    ~(f : 'a -> string)
    (xs : 'a list)
  =
  let extra =
    if String.equal obrace ""
    then extra
    else if String.is_substring sep ~substring:"\n"
    then string_of_indent (String.length obrace + 1) ^ extra
    else extra in
  let content =
    match xs with
    | [] -> ""
    | [ x ] -> f x
    | x :: nxs ->
      let sxs =
        let sx = f x in
        let snxs = List.map ~f:(fun u -> indent ^ extra ^ f x) nxs in
        sx :: snxs in
      String.concat ~sep sxs in
  let obrace, cbrace =
    if (not (String.is_substring content ~substring:"\n"))
       || String.equal obrace ""
    then obrace, cbrace
    else indent ^ obrace ^ " ", " " ^ cbrace in
  obrace ^ content ^ cbrace
;;

let sprint_list_square = sprint_list ~obrace:"[" ~cbrace:"]"
let sprint_list_curly = sprint_list ~obrace:"{" ~cbrace:"}"
let sprint_list_paren = sprint_list ~obrace:"(" ~cbrace:")"
let sprint_list_plain = sprint_list ~obrace:"" ~cbrace:""

let sprint_items
    ?(bullet = "-")
    ?(obrace = "")
    ?(cbrace = "")
    ?(extra = "")
    ~(f : 'a -> string)
    (items : 'a list)
  =
  if List.is_empty items
  then "[]"
  else (
    let indent = String.length bullet + 1 in
    let print x =
      let res = indent_line ~skipfirst:true indent (f x) in
      bullet ^ " " ^ res in
    let res = sprint_list ~sep:"\n" ~obrace ~cbrace ~extra ~f items in
    "\n" ^ res)
;;

let sprint_args ~(f : 'a -> string) (args : 'a list) : string =
  sprint_list ~sep:", " ~obrace:"" ~cbrace:"" ~f args
;;

let sprint_pair ~(f1: 'a -> string) ~(f2: 'b -> string) (p: 'a * 'b) : string =
  let x, y = p in
  "(" ^ f1 x ^ ", " ^ f2 y ^ ")"

let beautiful_concat ?(column = 80) ~(sep : string) (strs : string list) =
  let rec concat strs current_line res =
    match strs with
    | [] ->
      if String.is_empty current_line
      then res
      else if String.is_empty res
      then current_line
      else String.strip res ^ "\n" ^ current_line
    | str :: nstrs ->
      let new_line =
        if List.is_empty nstrs
        then current_line ^ str
        else current_line ^ str ^ sep in
      if String.length new_line <= column
      then concat nstrs new_line res
      else (
        let res =
          if String.is_empty res
          then current_line
          else String.strip res ^ "\n" ^ current_line in
        if List.is_empty nstrs
        then concat nstrs str res
        else concat nstrs (str ^ sep) res) in
  concat strs "" ""
;;

let beautiful_format_on_char ~(sep : char) ?(column = 80) (str : string) =
  let strs = String.split ~on:sep str in
  beautiful_concat ~sep:(Char.to_string sep) strs
;;
