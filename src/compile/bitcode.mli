(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

val print_module_stats : string -> unit
val export_bitcode_to_file : string -> Llvm.llmodule -> unit
val process_module : string -> Llir.llmodule -> Llir.program
val disassemble_bitcode : string -> unit
val optimize_bitcode : string -> string
val normalize_bitcode : string -> string
val process_bitcode : string -> Llir.program
val compile_program : string -> Llir.program
