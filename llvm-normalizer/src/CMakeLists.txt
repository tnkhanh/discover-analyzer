####################################################################
# This file is part of the tool Normalizer of the project Discover.
#
# Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
# All rights reserved.
####################################################################

set(LLNormCore
  Common.cpp
  Normalizer.cpp)

# Relevant LLVM Components can be found by: llvm-config --components
llvm_map_components_to_libnames(LLVM_LIBS
  asmparser core bitreader
  bitwriter irreader ipo scalaropts)

# add directory of normalization passes
add_subdirectory(Passes)
add_subdirectory(Debug)

# message(STATUS "Normalization Passes: ${LLNormPasses}")
# message(STATUS "Normalization Passes: ${LLNormDebug}")

set(LLNormSource ${LLNormCore} ${LLNormDebug} ${LLNormPasses})
# message(STATUS "Source Code: ${LLNormSource}")

if (APPLE)
  # compile to a binary file
  add_executable(normalizer ${LLNormSource})
else()
  # compile to a binary file
  add_executable(normalizer ${LLNormSource} ${BACKWARD_ENABLE})
  add_backward(normalizer)
endif()

# set_target_properties(NormalizerPass PROPERTIES
#   COMPILE_FLAGS "-fno-rtti"
#   )

target_link_libraries(normalizer ${LLVM_LIBS})

# Deploy after build
add_custom_command(TARGET normalizer POST_BUILD
  COMMAND ${CMAKE_COMMAND}
  -E copy $<TARGET_FILE:normalizer> ${PROJECT_BINARY_DIR}/normalizer
  COMMENT "Copied to: ${PROJECT_BINARY_DIR}/normalizer")
