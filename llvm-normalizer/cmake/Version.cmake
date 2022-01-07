####################################################################
# This file is part of the tool Normalizer of the project Discover.
#
# Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
# All rights reserved.
####################################################################

execute_process(COMMAND
  git log --pretty=format:'%h' -n 1 ${CMAKE_CURRENT_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_REV
  ERROR_QUIET)

# Check whether we got any revision
if ("${GIT_REV}" STREQUAL "")
  set(GIT_REV "N/A")
  set(GIT_DIFF "")
  set(GIT_TAG "N/A")
  set(GIT_BRANCH "N/A")
  set(GIT_TIME "N/A")
else()
  string(STRIP "${GIT_REV}" GIT_REV)
  string(SUBSTRING "${GIT_REV}" 1 7 GIT_REV)

  execute_process(
    COMMAND bash -c "git diff --quiet --exit-code"
    OUTPUT_VARIABLE GIT_DIFF)
  execute_process(
    COMMAND git describe --exact-match --tags
    OUTPUT_VARIABLE GIT_TAG ERROR_QUIET)
  execute_process(
    COMMAND git rev-parse --abbrev-ref HEAD
    OUTPUT_VARIABLE GIT_BRANCH)
  execute_process(
    COMMAND git show -s --format=%ci ${GIT_REV}
    OUTPUT_VARIABLE GIT_TIME)

  string(STRIP "${GIT_DIFF}" GIT_DIFF)
  string(STRIP "${GIT_TAG}" GIT_TAG)
  string(STRIP "${GIT_BRANCH}" GIT_BRANCH)
  string(STRIP "${GIT_TIME}" GIT_TIME)
endif()

set(VERSION
  "// This file is auto-genrated by Version.cmake;
const char* VERSION=\"0.1\";
const char* GIT_REV=\"${GIT_REV}${GIT_DIFF}\";
const char* GIT_TAG=\"${GIT_TAG}\";
const char* GIT_BRANCH=\"${GIT_BRANCH}\";
const char* GIT_TIME=\"${GIT_TIME}\";\n")

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/include/Version.h)
  file(READ ${CMAKE_CURRENT_SOURCE_DIR}/include/Version.h VERSION_)
else()
  set(VERSION_ "")
endif()

if (NOT "${VERSION}" STREQUAL "${VERSION_}")
  message(STATUS "Write Git revision to: ${CMAKE_CURRENT_SOURCE_DIR}/include/Version.h")
  file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/include/Version.h "${VERSION}")
endif()
