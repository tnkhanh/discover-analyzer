# Find the native LLVM includes and libraries
#
# Defines the following variables
#  LLVM_VERSION       - where to find llvm include files
#  LLVM_INCLUDE_DIRS   - where to find llvm include files
#  LLVM_LIBS
#
# This module reads hints about LLVM location from the file LLVM_ROOT
# and find relevant LLVM files using the command llvm-config
#
# Author: Ta Quang Trung
# Date:   July 7th, 2020


# first unset some variables
unset(LLVM_CONFIG CACHE)

# Find LLVM_ROOT and LLVM_CONFIG
if (LLVM_ROOT)
  message(STATUS "LLVM_ROOT is predefined to: " ${LLVM_ROOT})
  find_program(LLVM_CONFIG
    NAMES llvm-config HINTS ${LLVM_ROOT}/bin
    DOC "Find llvm-config file" NO_DEFAULT_PATH)
  if (NOT LLVM_CONFIG)
    message (FATAL_ERROR "Invalid LLVM_ROOT")
  endif()
else()
  message("Automatically find LLVM_ROOT")
  set(LLVM_CONFIG llvm-config)
  execute_process(COMMAND ${LLVM_CONFIG} --prefix
    OUTPUT_VARIABLE LLVM_ROOT
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if (NOT LLVM_ROOT)
    message (FATAL_ERROR "Could not find LLVM location")
  endif()
endif()

# find LLVM_VERSION
execute_process(COMMAND ${LLVM_CONFIG} --version
  OUTPUT_VARIABLE LLVM_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# find LLVM_INCLUDE_DIRS
execute_process(COMMAND ${LLVM_CONFIG} --includedir
  OUTPUT_VARIABLE LLVM_INCLUDE_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# find LLVM_LIBS
execute_process(
  COMMAND ${LLVM_CONFIG} --libfiles
  OUTPUT_VARIABLE LLVM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

message(STATUS "Found LLVM: ${LLVM_VERSION}")
message(STATUS "   Config file:    ${LLVM_CONFIG}")
message(STATUS "   Include dirs:   ${LLVM_INCLUDE_DIRS}")
message(STATUS "   LLVM libraries: ${LLVM_LIBS}")
