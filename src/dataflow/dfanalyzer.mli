(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module BM = Benchmark

type dfa_result =
  { dfa_total_analysis_time : float;
    dfa_detailed_analysis_time : (string * float) list;
    dfa_num_detected_bugs : int;
    dfa_detailed_detected_bugs : (string * int) list;
    dfa_num_valid_asserts : int;
    dfa_num_invalid_asserts : int
  }

val analyze_program : Llir.program -> (dfa_result * BM.benchmark_result)
