(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(* Printer module *)

open Core
open Sprinter
open Debugger
open Lib


(*******************************************************************
 ** Printing functions
 *******************************************************************)

let ruler_long   = "\n************************************************\n"
let ruler_medium = "\n*********************************\n"
let ruler_short  = "\n--------------------\n"

(** core printing function *)

let print_core ?(ruler=`None) ?(prefix="") ?(indent=0)
      ?(always=false) ?(format=true) msg : unit =
  if not !no_print || always then
    let msg = match ruler with
      | `Header -> ruler_long ^ (String.suffix_if_not_empty prefix ~suffix:"\n\n") ^ msg
      | `Long -> ruler_long ^ prefix ^ msg
      | `Medium -> ruler_medium ^ prefix ^ msg
      | `Short -> ruler_short ^ prefix ^ msg
      | `None ->
        let msg =
          if not format then msg
          else
          if String.is_prefix ~prefix:"\n" msg ||
             (String.length prefix > 1 && String.is_suffix ~suffix:"\n" prefix) then
            let indent = (get_indent prefix) + 2 + indent in
            prefix ^ (pr_indent indent msg)
          else if (String.length prefix > 12) &&
                  (String.is_infix ~infix:"\n" msg)then
            let indent = (get_indent prefix) + 2 + indent in
            prefix ^ "\n" ^ (pr_indent indent msg)
          else pr_indent indent (pr_align prefix msg) in
        if is_debug_mode () then "\n" ^ msg
        else msg in
    print_endline msg
  else ()

(** print a message *)

let print ?(ruler=`None) ?(indent=0) ?(always=false) ?(format=true) msg : unit =
  print_core ~ruler ~indent ~always ~format msg

let println ?(header="") ?(ruler=`None) ?(indent=0) ?(always=false) ?(format=true) msg : unit =
  let msg = if is_debug_mode () then msg else msg ^ "\n" in
  print_core ~ruler ~indent ~always ~format msg


(** high-order print a message *)

let hprint ?(ruler=`None) ?(indent=0) ?(always=false) ?(format=true)
      prefix (f: 'a -> string) (v: 'a) =
  print_core ~ruler ~indent ~prefix ~always ~format (f v)

let hprintln ?(ruler=`None) ?(indent=0) ?(always=false) ?(format=true)
      prefix (f: 'a -> string) (v: 'a) =
  let msg = f v in
  let msg = if is_debug_mode () then msg else msg ^ "\n" in
  print_core ~ruler ~indent ~prefix ~always ~format msg

let nprint _ = ()
let nhprint _ _ = ()
