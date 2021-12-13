#!/bin/sh

../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter --dfa-path-sensitive=false --dfa-context-split-phi=true" basic_c_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter --dfa-path-sensitive=false --dfa-context-split-phi=true" cs_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter --dfa-path-sensitive=false --dfa-context-split-phi=true" fs_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter --dfa-path-sensitive=false --dfa-context-split-phi=true" fstbhc_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter --dfa-path-sensitive=false --dfa-context-split-phi=true" path_tests/
