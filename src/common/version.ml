(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module PS = Outils.Process

let discover_official_version = "1.0"
let discover_git_revision = ref ""

let get_current_revision () : string =
  let rev =
    PS.run_command_get_output [ "git"; "rev-parse"; "--short"; "HEAD" ] in
  match rev with
  | Error _ -> "<Unable to get Git revision>"
  | Ok rev ->
    let date =
      PS.run_command_get_output [ "git"; "show"; "-s"; "--format=%ci"; rev ]
    in
    (match date with
    | Ok date -> rev ^ " @ " ^ date
    | Error _ -> rev ^ " @ <unknown date>")
;;
