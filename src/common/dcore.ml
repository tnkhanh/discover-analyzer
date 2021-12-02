(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(** Core module of Discover. To be open by all other modules *)

include Core

(*---------------------
 * Extended libraries
 *--------------------*)

include Libint
include Libhashtbl
include Liblist
include Libmath
include Libstring
include Libsys

(*---------------------
 * Discover's modules
 *--------------------*)

include Global
include Printer
include Debugger
