(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

let pid_dummy = -1000

type process = {
  proc_exe : string;
  proc_cmd : string list;
  proc_pid : int;
  proc_in_channel : in_channel;
  proc_out_channel : out_channel;
  proc_err_channel : in_channel;
}

type process_output =
  | POutput of string          (* output *)
  | PError of string           (* error message *)

let mk_proc_dummy (cmd: string list) =
  { proc_exe = (try List.hd_exn cmd with _ -> "");
    proc_cmd = cmd;
    proc_pid = pid_dummy;
    proc_in_channel = stdin;
    proc_out_channel = stdout;
    proc_err_channel = stdin; }

let open_process cmd : (in_channel * out_channel * in_channel * int) =
  let (in_read, in_write) = Unix.pipe() in
  let (out_read, out_write) = Unix.pipe() in
  let (err_read, err_write) = Unix.pipe() in
  let in_channel = Unix.in_channel_of_descr in_read in
  let out_channel = Unix.out_channel_of_descr out_write in
  let err_channel = Unix.in_channel_of_descr err_read in
  let pid = match Unix.fork () with
    | `In_the_child ->
      (* NOTE: the flag "close_on_exec" might affect reading output/error *)
      let () = Unix.dup2 ~src:out_read ~dst:Unix.stdin ~close_on_exec:false () in
      let () = Unix.dup2 ~src:in_write ~dst:Unix.stdout ~close_on_exec:false () in
      let () = Unix.dup2 ~src:err_write ~dst:Unix.stderr ~close_on_exec:false () in
      (* if not cloexec then List.iter Unix.close toclose; *)
      let prog = List.hd_exn cmd in
      let _ = Unix.exec ~prog:prog ~argv:cmd () in
      0
    | `In_the_parent id -> Pid.to_int id in
  (* debug "Join process"; *)
  let _ = Unix.close out_read in
  let _ = Unix.close in_write in
  let _ = Unix.close err_write in
  (in_channel, out_channel, err_channel, pid)

let close_process proc : unit =
  try
    let _ = Unix.close (Unix.descr_of_out_channel proc.proc_out_channel) in
    let _ = Unix.close (Unix.descr_of_in_channel proc.proc_err_channel) in
    Signal.send_exn Signal.kill (`Pid (Pid.of_int proc.proc_pid))
  with
  | e ->
    try
      Unix.close (Unix.descr_of_in_channel proc.proc_in_channel);
    with e -> ()

let read_output proc : string =
  let rec read acc =
    try read (acc @ [(input_line proc.proc_in_channel)])
    with End_of_file -> acc in
  let res = String.concat ~sep:"\n" (read []) in
  res

let read_error proc : string =
  let rec read acc =
    try read (acc @ [input_line proc.proc_err_channel])
    with _ -> acc in
  let res = String.concat ~sep:"\n" (read []) in
  res

let send_input proc input =
  output_string proc.proc_out_channel input;
  flush proc.proc_out_channel

let start_process (cmd: string list) : process =
  try
    let inchn, outchn, errchn, npid = open_process cmd in
    { proc_exe = (try List.hd_exn cmd with _ -> "");
      proc_cmd = cmd;
      proc_pid = npid;
      proc_in_channel = inchn;
      proc_out_channel = outchn;
      proc_err_channel = errchn }
  with e -> flush stdout; flush stderr; raise e

let stop_process proc =
  close_process proc

let restart_process proc : process =
  let _ = stop_process proc in
  start_process proc.proc_cmd

let run_command (cmd: string list) : unit =
  let proc = start_process cmd in
  match Unix.waitpid (Pid.of_int proc.proc_pid) with
  | Ok _ -> (close_process proc)
  | Error e ->
    (* let msg = string_of_sexp (Unix.Exit_or_signal.sexp_of_error e) in *)
    let msg = read_error proc in
    let _ = close_process proc in
    let cmd = beautiful_concat ~column:80 ~sep:" " cmd in
    error ~log:msg ("Failed to run external command:\n\n" ^ (pr_indent 2 cmd))

let run_command_get_output (cmd: string list) : process_output =
  let proc = start_process cmd in
  match Unix.waitpid (Pid.of_int proc.proc_pid) with
  | Ok _ ->
    let output = (read_output proc) ^ (read_error proc) in
    let _ = close_process proc in
    (POutput output)
  | Error _ ->
    let msg = read_error proc in
    let _ = close_process proc in
    (PError msg)
