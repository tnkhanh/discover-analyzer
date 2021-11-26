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

type ruler =
  | RlLong
  | RlShort
  | RlMedium
  | RlHeader
  | RlNone

let no_print = ref false

(*******************************************************************
 ** String printing functions
 *******************************************************************)

(*---------------------------
 * Basic printing functions
 *--------------------------*)

let pr_bool : bool -> string = string_of_bool
let pr_float : float -> string = string_of_float
let pr_int : int -> string = string_of_int
let pr_int64 : int64 -> string = Int64.to_string
let pr_str : string -> string = fun s -> s

(*------------------
 * Print to string
 *-----------------*)

let sprintf = Printf.sprintf

(** print a list to string *)
let pr_list
    ?(sep = ", ")
    ?(obrace = "[")
    ?(cbrace = "]")
    ?(indent = "")
    ?(extra = "")
    ~(f : 'a -> string)
    (xs : 'a list)
  =
  let xs = List.map ~f xs in
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
      let xs = x :: List.map ~f:(fun u -> indent ^ extra ^ u) nxs in
      String.concat ~sep xs in
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

(** print a list of items to string in itemized format *)
let pr_items
    ?(bullet = "-")
    ?(obrace = "")
    ?(cbrace = "")
    ?(extra = "")
    ~(f : 'a -> string)
    (xs : 'a list)
  =
  let xs = List.map ~f xs in
  if List.is_empty xs
  then "[]"
  else (
    let indent = String.length bullet + 1 in
    let pr s =
      let res = String.indent ~skipfirst:true indent s in
      bullet ^ " " ^ res in
    "\n" ^ pr_list ~sep:"\n" ~obrace ~cbrace ~extra ~f:pr xs)
;;

let pr_args ~(f : 'a -> string) (args : 'a list) : string =
  pr_list ~sep:", " ~obrace:"" ~cbrace:"" ~f args
;;

let pr_pair ~(f1 : 'a -> string) ~(f2 : 'b -> string) (p : 'a * 'b) : string =
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

(** core printing function *)
let print_core
    ?(header = false)
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
      if header
      then (
        let msg = if String.is_empty msg then msg else "\n\n" ^ msg ^ "\n\n" in
        "\n" ^ String.make 68 '*' ^ "\n" ^ prefix ^ msg)
      else (
        match ruler with
        | `Long -> "\n" ^ String.make 68 '*' ^ "\n" ^ prefix ^ msg
        | `Medium -> "\n" ^ String.make 36 '=' ^ "\n" ^ prefix ^ msg
        | `Short -> "\n" ^ String.make 21 '-' ^ "\n" ^ prefix ^ msg
        | `None ->
          let msg = "[info] " ^ msg in
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
          else String.indent indent (String.align_line prefix msg)) in
    print_endline msg)
  else ()
;;

(** print a message *)
let print
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msg : string)
    : unit
  =
  print_core ~header ~ruler ~indent ~always ~format msg
;;

(** print 2 messages *)
let print2
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msg1 : string)
    (msg2 : string)
    : unit
  =
  print_core ~header ~ruler ~indent ~always ~format (msg1 ^ msg2)
;;

(** print a list of messages *)
let printl
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msgs : string list)
    : unit
  =
  print_core ~header ~ruler ~indent ~always ~format (String.concat msgs)
;;

(** print a message and a newline character *)
let println
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (msg : string)
    : unit
  =
  print_core ~header ~ruler ~indent ~always ~format (msg ^ "\n")
;;

(** high-order print a message *)
let hprint
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(format = true)
    (prefix : string)
    (f : 'a -> string)
    (v : 'a)
  =
  print_core ~header ~ruler ~indent ~prefix ~always ~format (f v)
;;

let nprint _ = ()
let nprint2 _ = ()
let nprintl _ = ()
let nprintln _ = ()
let nhprint _ _ = ()

(** Print error *)

let eprint (msg : string) : unit = prerr_endline msg
let eprintf = Printf.eprintf

(*******************************************************************
 ** Warning and error
 *******************************************************************)

(** report a warning message *)
let warning msg =
  let msg = "[warning] " ^ msg in
  if not !print_concise_output then prerr_endline msg
;;

(** report 2 warning messages *)
let warning2 (msg1 : string) (msg2 : string) = warning (msg1 ^ msg2)

(** report a list of warning messages *)
let warningl (msgs : string list) = warning (String.concat ~sep:"" msgs)

(** high-order report a warning message *)
let hwarning (msg : string) (f : 'a -> string) (x : 'a) =
  warning (msg ^ f x)
;;

(** report an error message *)
let error ?(log = "") (msg : string) = raise (EError (msg, log))

(** report 2 error messages *)
let error2 ?(log = "") (msg1 : string) (msg2 : string) =
  let msg = msg1 ^ msg2 in
  error ~log msg
;;

(** report a list of error messages *)
let errorl ?(log = "") (msgs : string list) =
  let msg = String.concat ~sep:"" msgs in
  error ~log msg
;;

(** high-order report an error message *)
let herror (msg : string) (f : 'a -> string) (x : 'a) =
  let msg = msg ^ f x in
  error msg
;;

(*******************************************************************
 ** Override default printing function to throw some warning
 *******************************************************************)

let print_endline (s : string) =
  let _ =
    warning
      ("DO NOT USE print_endline DIRECTLY. "
     ^ "Use printing functions in Printer.ml instead!") in
  Core.print_endline s
;;

let print_string (s : string) =
  let _ =
    warning
      ("DO NOT USE print_string directly. \n"
     ^ "Use printing functions in Printer.ml instead!") in
  Core.print_string s
;;
