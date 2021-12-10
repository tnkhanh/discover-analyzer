(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(* Printer module *)
open Core
open Libstring

(*------------------
 * debugging flags
 *-----------------*)

let mode_debug = ref false
let mode_deep_debug = ref false
let no_debug = ref false

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

(** core mode_debug function *)
let debug_core
    ?(header = false)
    ?(ruler = `None)
    ?(prefix = "")
    ?(indent = 0)
    ?(marker = true)
    ?(enable = true)
    (print_msg : unit -> string)
  =
  if enable
  then (
    let msg = print_msg () in
    let str_marker = if marker then "[debug] " else "" in
    let msg =
      if header
      then (
        let prefix = String.suffix_if_not_empty prefix ~suffix:"\n" in
        "\n" ^ String.make 68 '*' ^ "\n" ^ str_marker ^ prefix ^ msg)
      else (
        match ruler with
        | `Long ->
          let ruler = String.make 68 '*' ^ "\n" in
          "\n" ^ ruler ^ "\n" ^ str_marker ^ prefix ^ msg
        | `Medium ->
          let ruler = String.make 36 '*' in
          "\n" ^ ruler ^ "\n" ^ str_marker ^ prefix ^ msg
        | `Short ->
          let ruler = String.make 21 '-' in
          "\n" ^ ruler ^ "\n" ^ str_marker ^ prefix ^ msg
        | `None ->
          let msg = if marker then msg else msg in
          if String.is_prefix ~prefix:"\n" msg
             || (String.length prefix > 1
                && String.is_suffix ~suffix:"\n" prefix)
          then (
            let indent = String.count_indent prefix + 2 + indent in
            str_marker ^ prefix ^ String.indent indent msg)
          else if String.length prefix > 12 && String.is_infix ~infix:"\n" msg
          then (
            let indent = String.count_indent prefix + 2 + indent in
            str_marker ^ prefix ^ "\n" ^ String.indent indent msg)
          else
            String.indent indent (String.align_line (str_marker ^ prefix) msg))
    in
    print_endline msg)
  else ()
;;

(*** simple debugging printers ***)

(** print a message *)
let debug
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    (msg : string)
    : unit
  =
  let enable =
    enable && (not !no_debug) && (!mode_debug || !mode_deep_debug || always)
  in
  let prefix = if ruler != `None then "" else if marker then "" else "\n" in
  debug_core ~header ~ruler ~indent ~enable ~prefix (fun () -> msg)
;;

(*** deep debugging printers ***)

(** print a deep mode_debug message *)
let ddebug
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    (msg : string)
    : unit
  =
  let enable = enable && (not !no_debug) && (!mode_deep_debug || always) in
  let printer () = msg in
  debug_core ~header ~ruler ~indent ~enable ~prefix:msg ~marker printer
;;

(*** higher order debugging printer ***)

(** high-order print a mode_debug message *)
let hdebug
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    (msg : string)
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable =
    enable && (not !no_debug) && (!mode_debug || !mode_deep_debug || always)
  in
  let printer () = pr data in
  debug_core ~header ~ruler ~indent ~enable ~prefix:msg ~marker printer
;;

(** high-order print a deep mode_debug message *)
let hddebug
    ?(header = false)
    ?(ruler = `None)
    ?(indent = 0)
    ?(always = false)
    ?(enable = true)
    ?(marker = true)
    (msg : string)
    (pr : 'a -> string)
    (data : 'a)
  =
  let enable = enable && (not !no_debug) && (!mode_deep_debug || always) in
  let printer () = pr data in
  debug_core ~header ~ruler ~indent ~enable ~marker ~prefix:msg printer
;;

(*** disable debugging printers ***)

let ndebug _ = ()
let nhdebug _ _ _ = ()
let nhddebug _ _ _ = ()

(*******************************************************************
 ** Interactive debugging
 *******************************************************************)

(** display choices and return a range *)

let display_choices msg (pr_choice : 'a -> string) (choices : 'a list) : string
  =
  let all_choices =
    choices
    |> List.mapi ~f:(fun idx c ->
           " [" ^ string_of_int (idx + 1) ^ "]. " ^ pr_choice c)
    |> String.concat ~sep:"\n" in
  let _ = print_endline (msg ^ "\n" ^ all_choices) in
  let range =
    let num_choices = List.length choices in
    if num_choices = 1 then "1" else "1" ^ ".." ^ string_of_int num_choices
  in
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
          with _ -> false))
      answer_choices in
  if not is_valid_choice
  then (
    let _ = print_endline "\n>>> Choose again!" in
    ask_decision question answer_choices)
  else answer
;;

let nask_decision _ _ = ()
