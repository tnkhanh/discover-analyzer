(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

type benchmark_result =
  { ben_correct_bug_reports : int;
    ben_incorrect_bug_reports : int;
    ben_missing_bugs : int;
    ben_detailed_result : string
  }

val compute_benchmark_result : Llir.program -> Bug.bug list -> benchmark_result
