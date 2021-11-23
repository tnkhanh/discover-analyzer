(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(* Printer module *)

open Core
open Globals
open Libstring
open Libcore

let no_print = ref false

(*******************************************************************
 ** String printing functions
 *******************************************************************)

(*----------------------
 * Compute indentation
 *---------------------*)

(*---------------------------
 * Basic printing functions
 *--------------------------*)

let sprint_bool : bool -> string = string_of_bool
let sprint_float : float -> string = string_of_float
let sprint_int : int -> string = string_of_int
let sprint_int64 = Int64.to_string

(*------------------
 * Print to string
 *-----------------*)

let sprintf = Printf.sprintf

(** print a list of string to string *)
let sprint_string_list
    ?(sep = ", ")
    ?(obrace = "[")
    ?(cbrace = "]")
    ?(indent = "")
    ?(extra = "")
    (xs : string list)
  =
  let extra =
    if String.equal obrace ""
    then extra
    else if String.is_substring sep ~substring:"\n"
    then String.mk_indent (String.length obrace + 1) ^ extra
    else extra in
  let content =
    match xs with
    | [] -> ""
    | [ x ] -> x
    | x :: nxs ->
      let sxs = x :: List.map ~f:(fun u -> indent ^ extra ^ u) nxs in
      String.concat ~sep sxs in
  let obrace, cbrace =
    if (not (String.is_substring content ~substring:"\n"))
       || String.equal obrace ""
    then obrace, cbrace
    else indent ^ obrace ^ " ", " " ^ cbrace in
  obrace ^ content ^ cbrace
;;

(** higher-order function to print a list to string *)
let sprint_list
    ?(sep = ", ")
    ?(obrace = "[")
    ?(cbrace = "]")
    ?(indent = "")
    ?(extra = "")
    ~(f : 'a -> string)
    (xs : 'a list)
  =
  let sxs = List.map ~f xs in
  sprint_string_list ~sep ~obrace ~cbrace ~indent ~extra sxs
;;

let sprint_list_square = sprint_list ~obrace:"[" ~cbrace:"]"
let sprint_list_curly = sprint_list ~obrace:"{" ~cbrace:"}"
let sprint_list_paren = sprint_list ~obrace:"(" ~cbrace:")"
let sprint_list_plain = sprint_list ~obrace:"" ~cbrace:""

(** print a list of string to string in itemized format *)
let sprint_string_list_itemized
    ?(bullet = "-")
    ?(obrace = "")
    ?(cbrace = "")
    ?(extra = "")
    (xs : string list)
  =
  if List.is_empty xs
  then "[]"
  else (
    let indent = String.length bullet + 1 in
    let sprint x =
      let res = String.indent ~skipfirst:true indent x in
      bullet ^ " " ^ res in
    "\n" ^ sprint_list ~sep:"\n" ~obrace ~cbrace ~extra ~f:sprint xs)
;;

(** higher-order function to print a list to string in itemized format *)
let hsprint_list_itemized
    ?(bullet = "-")
    ?(obrace = "")
    ?(cbrace = "")
    ?(extra = "")
    ~(f : 'a -> string)
    (xs : 'a list)
  =
  let sxs = List.map ~f xs in
  sprint_string_list_itemized ~bullet ~obrace ~cbrace ~extra sxs
;;

let sprint_args ~(f : 'a -> string) (args : 'a list) : string =
  sprint_list ~sep:", " ~obrace:"" ~cbrace:"" ~f args
;;

let sprint_pair ~(f1 : 'a -> string) ~(f2 : 'b -> string) (p : 'a * 'b)
    : string
  =
  let x, y = p in
  "(" ^ f1 x ^ ", " ^ f2 y ^ ")"
;;

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

(*******************************************************************
 ** Stdout printing functions
 *******************************************************************)

let ruler_long = "\n************************************************\n"
let ruler_medium = "\n*********************************\n"
let ruler_short = "\n--------------------\n"

(** core printing function *)

let print_core
    ?(ruler = `None)
    ?(prefix = "")
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    msg
    : unit
  =
  if (not !no_print) || always
  then (
    let msg =
      match ruler with
      | `Header ->
        let msg = if String.is_empty msg then msg else "\n\n" ^ msg ^ "\n" in
        ruler_long ^ prefix ^ msg
      | `Long -> ruler_long ^ prefix ^ msg
      | `Medium -> ruler_medium ^ prefix ^ msg
      | `Short -> ruler_short ^ prefix ^ msg
      | `None ->
        if not format
        then msg
        else if String.is_prefix ~prefix:"\n" msg
                || (String.length prefix > 1
                   && String.is_suffix ~suffix:"\n" prefix)
        then (
          let indent = String.count_indent prefix + 2 + indent in
          prefix ^ String.indent indent msg)
        else if String.length prefix > 12
                && String.exists ~f:(fun c -> c == '\n') msg
        then (
          let indent = String.count_indent prefix + 2 + indent in
          prefix ^ "\n" ^ String.indent indent msg)
        else String.indent indent (String.align_line prefix msg) in
    print_endline ("[info] " ^ msg))
  else ()
;;

(** print a message *)

let print
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msg : string)
    : unit
  =
  print_core ~ruler ~indent ~always ~format msg
;;

(** print 2 messages *)

let print2
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msg1 : string)
    (msg2 : string)
    : unit
  =
  print_core ~ruler ~indent ~always ~format (msg1 ^ msg2)
;;

(** print a list of messages *)

let prints
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msgs : string list)
    : unit
  =
  print_core ~ruler ~indent ~always ~format (String.concat msgs)
;;

let println
    ?(header = "")
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    msg
    : unit
  =
  print_core ~ruler ~indent ~always ~format (msg ^ "\n")
;;

(** high-order print a message *)

let hprint
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    prefix
    (f : 'a -> string)
    (v : 'a)
  =
  print_core ~ruler ~indent ~prefix ~always ~format (f v)
;;

let hprintln
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    prefix
    (f : 'a -> string)
    (v : 'a)
  =
  let msg = f v in
  print_core ~ruler ~indent ~prefix ~always ~format (msg ^ "\n")
;;

let nprint _ = ()
let nhprint _ _ = ()

(** Print error *)

let eprint (msg : string) : unit = prerr_endline msg
let eprintf = Printf.eprintf


(*******************************************************************
 ** Warning and error
 *******************************************************************)

let warning msg =
  let msg = "[warning] " ^ msg in
  if not !print_concise_output then prerr_endline msg
;;

let hwarning msg f x =
  let msg = msg ^ ": " ^ f x in
  warning msg
;;

(** report an error message *)

let error ?(log = "") (msg : string) = raise (EError (msg, log))

(** report 2 error messages *)

let error2 ?(log = "") (msg1 : string) (msg2 : string) =
  let msg = msg1 ^ msg2 in
  error ~log msg
;;

(** report a list of error messages *)

let errors ?(log = "") (msgs : string list) =
  let msg = String.concat ~sep:"" msgs in
  error ~log msg
;;

let herror msg f x =
  let msg = msg ^ f x in
  error msg
;;

(*******************************************************************
 ** Warning and error
 *******************************************************************)

(* let print_endline (str : string) : unit = *)
(*   let _ = print_endline str in *)
(*   () *)
(* ;; *)
