(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

type symexec_result

val compile_sep_logic : string -> Slir.program
val verify_program : Slir.program -> symexec_result
val analyze_program : Llir.program -> symexec_result
