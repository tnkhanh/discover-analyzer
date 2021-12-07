(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module Entails :
sig
  val dump_data_defn : Slir.data_defn -> string
  val dump_view_defn : Slir.view_defn -> string
  val dump_func_defn : Slir.func_defn -> string
  val dump_reln_defn : Slir.reln_defn -> string
  val dump_command : Slir.command -> string
  val dump_program : Slir.program -> string
  val export_program : ?export:bool -> Slir.program -> unit
  val export_entailments : ?export:bool -> Slir.entailments -> unit
end
