####################################################################
# This file is part of the tool Normalizer of the project Discover.
#
# Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
# All rights reserved.
####################################################################

project(llvm-normalizer)

cmake_minimum_required(VERSION 3.0.0)
set(CMAKE_CXX_STANDARD 17)
message(STATUS "Set C++ standard: C++-17")
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -frtti -fexceptions -fPIC")

# Determine the OS type
if(UNIX AND NOT APPLE)
  set(LINUX TRUE)
endif()

# Backward debugging trace is supported only in Linux
# if(LINUX)
  set(CMAKE_BUILD_TYPE Debug)
  # set(CMAKE_BUILD_TYPE RelWithDebInfo)
  set(CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "-rdynamic")
# endif()

# Set cmake dirs
set(
  CMAKE_DIR   "${CMAKE_CURRENT_LIST_DIR}/cmake"
  CACHE PATH  "The the path to the cmake directory")
list(APPEND CMAKE_MODULE_PATH ${CMAKE_DIR})

# Set LLVM version
set(LLVM_VERSION 13.0)
message (STATUS "Looking for LLVM ${LLVM_VERSION}")

# Set LLVM preferred path
set(LLVM_CMAKE_PATH_HINTS
  ## Linux
  "$HOME/llvm/llvm-12/lib/cmake/llvm"         # manually provided path
  "/usr/lib/llvm-12/lib/cmake/llvm/"          # system path
  # "/usr/lib/llvm-11/lib/cmake/llvm/"        # system path
  # "/usr/lib/llvm/lib/cmake/llvm/"
  # "/usr/lib/llvm-8/lib/cmake/llvm/"
  # "/usr/lib/llvm-10/lib/cmake/llvm/"
  ## MacOS
  # "/usr/local/opt/llvm/lib/cmake/llvm/"
  # "/usr/local/opt/llvm@8/lib/cmake/llvm/"
  # "/usr/local/opt/llvm@10/lib/cmake/llvm/"
  # "/usr/local/opt/llvm@11/lib/cmake/llvm/"
  )

# config llvm
find_package(LLVM ${LLVM_VERSION}
  HINTS ${LLVM_CMAKE_PATH_HINTS}
  REQUIRED CONFIG)

if (NOT LLVM_DIR)
  message (FATAL_ERROR "Could not find LLVM CMake files")
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "CMake files are located at: ${LLVM_DIR}")
message(STATUS "Using the file: ${LLVM_DIR}/LLVMConfig.cmake")

# The below LLVM variables are read from ${LLVM_DIR}/LLVMConfig.cmake
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
link_directories(${LLVM_LIBRARY_DIRS})

# Link LLVM libraries
link_libraries()
# execute_process(COMMAND llvm-config --libs OUTPUT_VARIABLE LLVM_LIBS)
# execute_process(COMMAND llvm-config --system-libs OUTPUT_VARIABLE LLVM_SYS_LIBS)
# execute_process(COMMAND llvm-config --ldflags OUTPUT_VARIABLE LLVM_LDF)
# string(STRIP ${LLVM_LIBS} LLVM_LIBS)
# string(STRIP ${LLVM_SYS_LIBS} LLVM_SYS_LIBS)
# string(STRIP ${LLVM_LDF} LLVM_LDF)
# message(STATUS "Using LLVM libraries: ${LLVM_LIBS}")
# message(STATUS "Using system libraries: ${LLVM_SYSTEM_LIBS}")
# message(STATUS "Using linker flags: ${LLVM_LDF}")
# link_libraries(${LLVM_LIBS} ${LLVM_SYS_LIBS} ${LLVM_LDF})
link_libraries(${LLVM_AVAILABLE_LIBS})

# Setting for third-party libraries
set(EXTRA_LIB_DIRS "${CMAKE_CURRENT_LIST_DIR}/lib")
include_directories(${EXTRA_LIB_DIRS})
message(STATUS "Found extra libraries at: ${EXTRA_LIB_DIRS}")

# # Backward debugging is available only in Linux
# if(LINUX)
add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/lib/backward")
# endif()

# include header files
include_directories(include)

# Now adding source code
add_subdirectory(src)
