(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(* Printer module *)

open Core
open Global
open Libstring
module FM = CamlinternalFormat
module FB = CamlinternalFormatBasics

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

(** print option *)
let pr_option ~(f : 'a -> string) (x : 'a option) =
  match x with
  | Some v -> "Some(" ^ f v ^ ")"
  | None -> "None"
;;

let pr_bool_option (x : bool option) = pr_option ~f:pr_bool x

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

(** print a list of items to string in itemized autoformat *)
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

let sprintf = Printf.sprintf

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
    ?(marker = true)
    ?(enable = true)
    ?(autoformat = true)
    msg
    : unit
  =
  if ((not !no_print) && enable) || always
  then (
    let str_marker = if marker then "[info] " else "" in
    let msg =
      if header
      then (
        let msg = if String.is_empty msg then msg else "\n\n" ^ msg ^ "\n\n" in
        "\n" ^ String.make 68 '*' ^ "\n" ^ str_marker ^ prefix ^ msg)
      else (
        match ruler with
        | `Long -> "\n" ^ String.make 68 '*' ^ "\n" ^ str_marker ^ prefix ^ msg
        | `Medium ->
          "\n" ^ String.make 36 '=' ^ "\n" ^ str_marker ^ prefix ^ msg
        | `Short ->
          "\n" ^ String.make 21 '-' ^ "\n" ^ str_marker ^ prefix ^ msg
        | `None ->
          if not autoformat
          then msg
          else if String.is_prefix ~prefix:"\n" msg
                  || (String.length prefix > 1
                     && String.is_suffix ~suffix:"\n" prefix)
          then (
            let indent = String.count_indent prefix + 2 + indent in
            str_marker ^ prefix ^ String.indent indent msg)
          else if String.length prefix > 12
                  && String.exists ~f:(fun c -> c == '\n') msg
          then (
            let indent = String.count_indent prefix + 2 + indent in
            str_marker ^ prefix ^ "\n" ^ String.indent indent msg)
          else
            String.indent indent (String.align_line (str_marker ^ prefix) msg))
    in
    print_endline msg)
  else ()
;;

(** print a message *)
let print
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    ?(autoformat = true)
    (msg : string)
    : unit
  =
  print_core ~header ~ruler ~indent ~always ~enable ~marker ~autoformat msg
;;

(** print a message and a newline character *)
let println
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    ?(autoformat = true)
    (msg : string)
    : unit
  =
  let msg = msg ^ "\n" in
  print_core ~header ~ruler ~indent ~always ~enable ~marker ~autoformat msg
;;

(** high-order print a message *)
let hprint
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    ?(autoformat = true)
    (prefix : string)
    (f : 'a -> string)
    (v : 'a)
  =
  let msg = f v in
  print_core ~header ~ruler ~indent ~prefix ~always ~enable ~marker ~autoformat
    msg
;;

let nprint _ = ()
let nprintln _ = ()
let nhprint _ _ = ()

(*******************************************************************
 ** Warning printing
 *******************************************************************)

(** Print a warning message *)
let warning (msg : string) : unit =
  let msg = "[warning] " ^ msg in
  if not !print_concise_output then prerr_endline msg
;;

(** High-order print a warning message *)
let hwarning (msg : string) (f : 'a -> string) (x : 'a) : unit =
  warning (msg ^ f x)
;;

(** Print a warning message using output format template similar to printf. *)
let warningf fmt =
  let _ = print_string "[warning] " in
  let kwprintf k o (FB.Format (fmt, _)) =
    FM.make_printf (fun acc -> FM.output_acc o acc; k o) FM.End_of_acc fmt
  in
  kwprintf ignore stdout fmt
;;

(*******************************************************************
 ** Error printing
 *******************************************************************)

let print_error_log (log : string) : unit =
  if (not !release_mode) && String.not_empty log
  then print_endline ("\n[Error log]\n" ^ String.indent 2 log)
  else ()
;;

(** Report an error message and exit the program. *)
let error ?(log = "") (msg : string) : 't =
  let _ = print_endline ("ERROR: " ^ msg) in
  let _ = print_error_log log in
  exit 1
;;

(** High-order report an error message and exit the program. *)
let herror ?(log = "") (msg : string) (f : 'a -> string) (x : 'a) : 't =
  let msg = msg ^ f x in
  error ~log msg
;;

(** Report error using output format template similar to printf,
    and exit the program. *)
let errorf ?(log = "") fmt =
  let _ = print_string "ERROR: " in
  let keprintf k o (FB.Format (fmt, _)) =
    let print_error_and_exit o acc =
      let _ = FM.output_acc o acc in
      let _ = print_error_log log in
      ignore (exit 1) in
    FM.make_printf
      (fun acc -> print_error_and_exit o acc; k o)
      FM.End_of_acc fmt in
  keprintf ignore stdout fmt
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
