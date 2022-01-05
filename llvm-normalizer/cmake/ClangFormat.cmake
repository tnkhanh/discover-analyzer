####################################################################
# This file is part of the tool Normalizer of the project Discover.
#
# Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
# All rights reserved.
####################################################################

file(GLOB_RECURSE SOURCE_FILES
  ${PROJECT_SOURCE_DIR}/src/*.cpp
  ${PROJECT_SOURCE_DIR}/src/*.h)

# # debugging
# foreach (FILE ${SOURCE_FILES})
#   message (STATUS "Source file: ${FILE}")
# endforeach ()

add_custom_target(
  clangformat
  COMMAND clang-format
  -style=LLVM
  -i
  ${SOURCE_FILES})
