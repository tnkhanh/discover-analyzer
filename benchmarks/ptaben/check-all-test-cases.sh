#!/bin/sh

../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter" basic_c_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter" path_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter" cs_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter" fs_tests/
../../script/check-assertions.py --clang-option "-I ../../lib/discover/" --discover-option "--dfa-pointer --dfa-inter" fstbhc_tests/
