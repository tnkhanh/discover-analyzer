(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Sprinter
open Printer
open Debugger
module PS = Process

let discover_official_version = "1.0"
let discover_git_revision = ref ""

let get_current_revision () : string =
  let rev =
    PS.run_command_get_output [ "git"; "rev-parse"; "--short"; "HEAD" ] in
  match rev with
  | PError _ -> "<Unable to get Git revision>"
  | POutput rev ->
    let date =
      PS.run_command_get_output [ "git"; "show"; "-s"; "--format=%ci"; rev ]
    in
    (match date with
    | POutput date -> rev ^ " @ " ^ date
    | PError _ -> rev ^ " @ <unknown date>")
;;
