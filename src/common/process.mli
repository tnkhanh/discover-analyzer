(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

type process =
  { proc_exe : string;
    proc_cmd : string list;
    proc_pid : int;
    proc_in_channel : in_channel;
    proc_out_channel : out_channel;
    proc_err_channel : in_channel
  }

val pid_dummy : int

val mk_proc_dummy : string list -> process

(*** Handle input/output *)

val read_output : process -> string
val read_error : process -> string
val send_input : process -> string -> unit

(*** Start, stop processes ***)

val open_process : string list -> in_channel * out_channel * in_channel * int
val start_process : string list -> process
val close_process : process -> unit
val restart_process : process -> process

(*** Run commands ***)

val run_command : string list -> unit
val run_command_get_output : string list -> (string, string) result
