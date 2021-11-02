(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(* Printer module *)

open Core
open Sprinter
open Lib
open Libdiscover

(*------------------
 * debugging flags
 *-----------------*)

let mode_debug = ref false (* parameter: -d  *)

let mode_deep_debug = ref false (* parameter: -dd *)

let no_debug = ref false
let no_print = ref false

(*-------------------
 * Interactive mode
 *------------------*)

let mode_interactive = ref false
let mode_interactive_prover = ref false
let mode_interactive_verifier = ref false

(*----------------
 * Saving states
 *---------------*)

let saved_mode_debug = ref false
let saved_mode_deep_debug = ref false

(*----------------------
 * Debugging functions
 *---------------------*)

let regex_debug_function = ref ""
let mode_debug_function = ref false

(*------------------------------
 * Debugging working functions
 *-----------------------------*)

let regex_debug_working_function = ref ""
let mode_debug_working_function = ref false

(*******************************************************************
 ** Utility functions
 *******************************************************************)

let is_debug_mode () = !mode_debug || !mode_deep_debug
let is_interactive_mode () = !mode_interactive
let enable_mode_debug () = mode_debug := true
let disable_mode_debug () = mode_debug := false

let enable_mode_deep_debug () =
  mode_debug := true;
  mode_deep_debug := true
;;

let save_mode_debug () : unit =
  saved_mode_debug := !mode_debug;
  saved_mode_deep_debug := !mode_deep_debug
;;

let restore_mode_debug () : unit =
  mode_debug := !saved_mode_debug;
  mode_deep_debug := !saved_mode_deep_debug
;;

(*******************************************************************
 ** Debugging functions
 *******************************************************************)

(*----------------------
 * first-order printer
 *---------------------*)

let ruler_long = "\n************************************************\n"
let ruler_medium = "\n*********************************\n"
let ruler_short = "\n--------------------\n"

(** core mode_debug function *)

let debug_core
    ?(ruler = `None)
    ?(prefix = "")
    ?(indent = 0)
    ~enable
    (printer : unit -> string)
  =
  if enable
  then (
    let msg = printer () in
    let msg =
      match ruler with
      | `Header ->
        let prefix = String.suffix_if_not_empty prefix ~suffix:"\n\n" in
        ruler_long ^ prefix ^ msg
      | `Long -> ruler_long ^ prefix ^ msg
      | `Medium -> ruler_medium ^ prefix ^ msg
      | `Short -> ruler_short ^ prefix ^ msg
      | `None ->
        if String.is_prefix ~prefix:"\n" msg
           || (String.length prefix > 1 && String.is_suffix ~suffix:"\n" prefix)
        then (
          let indent = get_indent prefix + 2 + indent in
          prefix ^ pr_indent indent msg)
        else if String.length prefix > 12 && String.is_infix ~infix:"\n" msg
        then (
          let indent = get_indent prefix + 2 + indent in
          prefix ^ "\n" ^ pr_indent indent msg)
        else pr_indent indent (pr_align prefix msg) in
    print_endline msg)
  else ()
;;

(** print a mode_debug messages *)

let debug ?(ruler = `None) ?(indent = 0) ?(always = false) msg : unit =
  let enable = (not !no_debug) && (!mode_debug || !mode_deep_debug || always) in
  let prefix = if ruler == `None then "\n" else "" in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> msg)
;;

(** print a concise mode_debug message *)

let debugc ?(ruler = `None) ?(indent = 0) ?(always = false) msg : unit =
  let enable = (not !no_debug) && (!mode_debug || !mode_deep_debug || always) in
  debug_core ~ruler ~indent ~enable (fun () -> msg)
;;

(** print a mode_debug message and a new line *)

let debugln ?(ruler = `None) ?(indent = 0) ?(always = false) msg : unit =
  debug ~ruler ~indent ~always (msg ^ "\n")
;;

(** print a deep mode_debug message *)

let ddebug ?(ruler = `None) ?(indent = 0) ?(always = false) msg : unit =
  let enable = (not !no_debug) && (!mode_deep_debug || always) in
  debug_core ~ruler ~indent ~enable ~prefix:"\n" (fun () -> msg)
;;

(** print a concise deep mode_debug message *)

let ddebugc ?(ruler = `None) ?(indent = 0) ?(always = false) msg : unit =
  let enable = (not !no_debug) && (!mode_deep_debug || always) in
  debug_core ~ruler ~indent ~enable (fun () -> msg)
;;

(** print a mode_debug message and a new line*)

let ddebugln ?(ruler = `None) ?(indent = 0) ?(always = false) msg : unit =
  ddebug ~ruler ~indent ~always (msg ^ "\n")
;;

(* a *)

let ndebug _ = ()

(*** higher order printers ***)

(** high-order print a mode_debug message *)

let hdebug
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    msg
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = (not !no_debug) && (!mode_debug || !mode_deep_debug || always) in
  let prefix = "\n" ^ msg in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> pr data)
;;

(** high-order print a concise mode_debug message *)

let hdebugc
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    msg
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = (not !no_debug) && (!mode_debug || !mode_deep_debug || always) in
  let prefix = msg in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> pr data)
;;

(** high-order print a mode_debug message and a new line *)
let hdebugln
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    msg
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = (not !no_debug) && (!mode_debug || !mode_deep_debug || always) in
  let prefix = "\n" ^ msg in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> pr data ^ "\n")
;;

(** high-order print a deep mode_debug message *)

let hddebug
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    msg
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = (not !no_debug) && (!mode_deep_debug || always) in
  let prefix = "\n" ^ msg in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> pr data)
;;

(** high-order print a deep concise mode_debug message *)

let hddebugc
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    msg
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = (not !no_debug) && (!mode_deep_debug || always) in
  let prefix = msg in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> pr data)
;;

(** high-order print a deep mode_debug message *)

let hddebugln
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    msg
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = (not !no_debug) && (!mode_deep_debug || always) in
  let prefix = "\n" ^ msg in
  debug_core ~ruler ~indent ~enable ~prefix (fun () -> pr data ^ "\n")
;;

(*******************************************************************
 ** Interactive
 *******************************************************************)

(** display choices and return a range *)

let display_choices msg (pr_choice : 'a -> string) (choices : 'a list) : string =
  let all_choices =
    choices
    |> List.mapi ~f:(fun idx c -> " [" ^ pr_int (idx + 1) ^ "]. " ^ pr_choice c)
    |> String.concat ~sep:"\n" in
  let _ = print_endline (msg ^ "\n" ^ all_choices) in
  let range =
    let num_choices = List.length choices in
    if num_choices = 1 then "1" else "1" ^ ".." ^ pr_int num_choices in
  range
;;

(** answer choices can be in the form [s1; s2; "i..j"], where s1, s2 are
    some strings, and i, j are the lower and upper bound integer of a range *)

let rec ask_decision question (answer_choices : string list) : string =
  let all_choices = "[" ^ String.concat ~sep:"/" answer_choices ^ "]" in
  let msg = question ^ " " ^ all_choices ^ ": " in
  let _ = print_string ("\n$$$>>> " ^ msg) in
  let _ = flush_all () in
  let answer = String.strip In_channel.(input_line_exn stdin) in
  let is_valid_choice =
    List.exists
      ~f:(fun str ->
        if String.equal answer str
        then true
        else (
          try
            let index_answer = int_of_string answer in
            let regexp = Str.regexp "\\([0-9]+\\)\\.\\.\\([0-9]+\\)" in
            if Str.string_match regexp str 0
            then (
              let index_begin = int_of_string (Str.matched_group 1 str) in
              let index_end = int_of_string (Str.matched_group 2 str) in
              index_begin <= index_answer && index_answer <= index_end)
            else false
          with
          | _ -> false))
      answer_choices in
  if not is_valid_choice
  then (
    let _ = print_endline "\n>>> Choose again!" in
    ask_decision question answer_choices)
  else answer
;;

let nask_decision _ _ = ()
