(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Global
open Extcore.String

module FM = CamlinternalFormat
module FB = CamlinternalFormatBasics

let disable_warning = ref false

(*******************************************************************
 ** Warning printing
 *******************************************************************)

(** Print a warning message *)
let warning (msg : string) : unit =
  let msg = "[warning] " ^ msg in
  if not !disable_warning then prerr_endline msg
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
