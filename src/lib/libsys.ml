(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(*******************************************************************
 * Extending library Sys with new functionalities
 *******************************************************************)

module Sys = struct
  let track_runtime (f : unit -> 'a) : 'a * float =
    let time_begin = Unix.gettimeofday () in
    let res = f () in
    let time_end = Unix.gettimeofday () in
    let time = time_end -. time_begin in
    res, time
  ;;

  let report_runtime ?(task = "") (f : unit -> 'a) : 'a =
    let res =
      if String.is_empty task
      then f ()
      else (
        let time_begin = Unix.gettimeofday () in
        let res = f () in
        let time_end = Unix.gettimeofday () in
        let time = time_end -. time_begin in
        let _ = print_endline (task ^ ": " ^ sprintf "%.3fs" time) in
        res) in
    res
  ;;

  let record_runtime (f : unit -> 'a) (time : float ref) : 'a =
    let time_begin = Unix.gettimeofday () in
    let res = f () in
    let time_end = Unix.gettimeofday () in
    time := !time +. (time_end -. time_begin);
    res
  ;;

  let is_os_unix () : bool = String.equal Sys.os_type "Unix"

  let remove_dir (filename : string) : unit =
    if is_os_unix ()
    then ignore (Sys.command ("rm -rf " ^ filename))
    else prerr_endline ("TODO: remove_dir: need to support " ^ Sys.os_type)
  ;;

  let remove_file (filename : string) : unit =
    if is_os_unix ()
    then ignore (Sys.command ("rm -f " ^ filename))
    else prerr_endline ("TODO: remove_file: need to support " ^ Sys.os_type)
  ;;

  let make_dir (dirname : string) : unit =
    match Sys.file_exists dirname with
    | `No -> ignore (Sys.command ("mkdir -p " ^ dirname))
    | _ ->
      (match Sys.is_directory dirname with
      | `No ->
        print_endline
          (("'" ^ dirname ^ "' is an regular file. ")
          ^ "Discover needs to use this name to work.\n"
          ^ "Please remove or rename it to continue.\n");
        exit 0
      | _ -> ())
  ;;

  (*-------------------------------------
   * Include the existing Sys library
   *------------------------------------*)

  include Sys
end
