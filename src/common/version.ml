(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module PS = Outils.Process

let discover_official_version = "1.0"
let discover_git_revision = ref ""

let get_date_of_commit (commit_id : string) : string =
  let cmd = [ "git"; "show"; "-s"; "--format=%ci"; commit_id ] in
  match PS.run_command_get_output cmd with
  | Ok date -> date
  | Error _ -> "<unknown date>"
;;

let get_current_revision_info () : string =
  let cmd = [ "git"; "rev-parse"; "--short"; "HEAD" ] in
  match PS.run_command_get_output cmd with
  | Error _ -> "<Unable to get Git revision>"
  | Ok rev -> rev ^ " @ " ^ get_date_of_commit rev
;;

let get_latest_commit_info_of_file (filename : string) : string =
  let cmd = [ "git"; "log"; "-1"; "--pretty=format:\"%h\""; filename ] in
  match PS.run_command_get_output cmd with
  | Error _ -> "<Unable to get Git revision>"
  | Ok rev -> rev ^ " @ " ^ get_date_of_commit rev
;;
