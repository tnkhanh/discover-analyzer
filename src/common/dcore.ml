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

include Extcore.Libint
include Extcore.Libhashtbl
include Extcore.Liblist
include Extcore.Libmath
include Extcore.Libstring
include Extcore.Libsys
include Extcore.Libresult

(*---------------------
 * Discover's modules
 *--------------------*)

include Global
include Printer
include Debugger
