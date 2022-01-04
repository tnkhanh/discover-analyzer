(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

val pr_file_excerpt : string -> int -> int -> int -> int -> string
val pr_code_excerpt_and_location : ?func:string -> Global.position -> string
