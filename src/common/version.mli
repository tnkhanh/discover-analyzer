(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

val discover_official_version : string
val discover_git_revision : string ref
val get_current_revision_info : unit -> string
val get_latest_commit_info_of_file : string -> string
