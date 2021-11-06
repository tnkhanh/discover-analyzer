(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(*******************************************************************
 * Extending Hashtbl with new functionalities
 *******************************************************************)

module Hashtbl = struct
  let find_or_compute (tbl : ('a, 'b) Hashtbl.t) ~(key : 'a) ~(f : unit -> 'b)
    : 'b
    =
    match Hashtbl.find tbl key with
    | Some data -> data
    | None ->
      let data = f () in
      let _ = Hashtbl.set tbl ~key ~data in
      data
  ;;

  let find_default (tbl : ('a, 'b) Hashtbl.t) (key : 'a) ~(default : 'b) : 'b =
    match Hashtbl.find tbl key with
    | None -> default
    | Some v -> v
  ;;

  (* include the original Hashtbl *)
  include Hashtbl
end
