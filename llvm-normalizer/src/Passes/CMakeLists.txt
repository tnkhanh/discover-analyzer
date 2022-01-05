####################################################################
# This file is part of the tool Normalizer of the project Discover.
#
# Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
# All rights reserved.
####################################################################

set(LLNormPasses
  ${CMAKE_CURRENT_SOURCE_DIR}/InlineSimpleFunction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/ElimUnusedFunction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/ElimUnusedGlobal.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/ElimIdenticalInstrs.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/ElimAllocaStoreArg.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/CombineGEP.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/InitGlobal.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/UninlineInstruction.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/ElimInlineAsm.cpp
  PARENT_SCOPE)

# message(STATUS "LLNormPasses-orig: ${LLNormPasses}")
