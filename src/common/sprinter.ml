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
    with
    | EInt res -> res)
;;

let whitespace_of (length : int) : string =
  let rec mk_whitespace i = if i <= 0 then "" else " " ^ mk_whitespace (i - 1) in
  mk_whitespace length
;;

(*******************************************************************
 ** Basic printer
 *******************************************************************)

let pr_id = fun x -> x
let string_of_bool : bool -> string = string_of_bool
let string_of_float : float -> string = string_of_float
let string_of_int (x : int) : string = string_of_int x
let string_of_int64 = Int64.to_string
let string_of_string (s : string) : string = s

let sprintf = Printf.sprintf

(* error printing *)
let pr_error : string -> unit = prerr_endline

let pr_opt f x : string =
  match x with
  | None -> "None"
  | Some y -> "Some " ^ f y
;;

(** insert an indentation to each line of a message *)
let pr_indent ?(skipfirst = false) (indent : int) msg : string =
  let sindent = whitespace_of indent in
  msg
  |> String.split ~on:'\n'
  |> List.mapi ~f:(fun i s -> if i = 0 && skipfirst then s else sindent ^ s)
  |> String.concat ~sep:"\n"
;;

(** high-order insert an indentation to each line of a message *)
let hpr_indent ?(skipfirst = false) (i : int) (f : 'a -> string) (v : 'a)
    : string
  =
  pr_indent ~skipfirst i (f v)
;;

(** auto-insert indentation to pr_align with the prefix string *)
let pr_align prefix msg : string =
  let indent = String.length (String.strip_newline prefix) in
  let skipfirst = not (String.is_suffix ~suffix:"\n" prefix) in
  prefix ^ pr_indent ~skipfirst indent msg
;;

(** high-order auto-insert indentation to pr_align with the prefix string *)
let hpr_align prefix (f : 'a -> string) (v : 'a) : string =
  pr_align prefix (f v)
;;

(** insert a prefix to each line of a message *)
let pr_prefix ~(prefix : string) msg : string =
  msg
  |> String.split_lines
  |> List.map ~f:(fun s -> prefix ^ s)
  |> String.concat ~sep:"\n"
;;

let pr_list
    ?(sep = ", ")
    ?(obrace = "[")
    ?(cbrace = "]")
    ?(indent = "")
    ?(extra = "")
    print
    items
  =
  let extra =
    if String.equal obrace ""
    then extra
    else if String.is_substring sep ~substring:"\n"
    then whitespace_of (String.length obrace + 1) ^ extra
    else extra in
  let content =
    match items with
    | [] -> ""
    | [ u ] -> print u
    | u :: us ->
      let nsitems =
        let nu = print u in
        let nus = List.map ~f:(fun u -> indent ^ extra ^ print u) us in
        nu :: nus in
      String.concat ~sep nsitems in
  let obrace, cbrace =
    if (not (String.is_substring content ~substring:"\n"))
       || String.equal obrace ""
    then obrace, cbrace
    else indent ^ obrace ^ " ", " " ^ cbrace in
  obrace ^ content ^ cbrace
;;

let pr_list_square = pr_list ~obrace:"[" ~cbrace:"]"
let pr_list_curly = pr_list ~obrace:"{" ~cbrace:"}"
let pr_list_paren = pr_list ~obrace:"(" ~cbrace:")"
let pr_list_plain = pr_list ~obrace:"" ~cbrace:""

let pr_items
    ?(bullet = "-")
    ?(obrace = "")
    ?(cbrace = "")
    ?(extra = "")
    print
    items
  =
  if List.is_empty items
  then "[]"
  else (
    let indent = String.length bullet + 1 in
    let print x =
      let res = pr_indent ~skipfirst:true indent (print x) in
      bullet ^ " " ^ res in
    let res = pr_list ~sep:"\n" ~obrace ~cbrace ~extra print items in
    "\n" ^ res)
;;

let pr_args print items = pr_list ~sep:", " ~obrace:"" ~cbrace:"" print items
let pr_pair pr1 pr2 (v1, v2) = "(" ^ pr1 v1 ^ ", " ^ pr2 v2 ^ ")"

let pr_pairs pr1 pr2 items =
  let pr (v1, v2) = "(" ^ pr1 v1 ^ "," ^ pr2 v2 ^ ")" in
  pr_list ~sep:";" pr items
;;

let string_of_ints is = pr_list_square string_of_int is

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
