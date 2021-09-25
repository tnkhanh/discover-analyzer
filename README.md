Discover -- a source code static analyzer
=====================================================

*Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.*


# Compiling Discover

## Prerequisites

Preferably Ubuntu / Linux Mint. The following commands are tested and work well
with Ubuntu 21.04

### Tools and libraries

- CMake, zlib, libedit, z3, ninja

  ``` sh
  # Ubuntu, Linux Mint
  sudo apt-get install cmake zlib1g-dev libedit-dev z3 ninja-build

  # Arch Linux, Manjaro
  sudo pacman -S cmake zlib z3 libedit
  ```

- LLVM and Clang 13

  + Install from Linux repositories:

    ```
      To update when it is released
    ```

  + Or download prebuilt LLVM and Clang 
 
    ```
      To update when it is released
    ```

  + Otherwise, LLVM and Clang 13 can be built from source.
    To do this, download source code of LLVM 13 from [LLVM GitHub
    Releases](https://github.com/llvm/llvm-project/releases) and run 
    in the `llvm-project` directory:

    ``` sh
    mkdir -p build; cd build
    cmake ../llvm -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
          -DCMAKE_INSTALL_PREFIX=/path/to/your/installation -Wno-dev -G Ninja
    ninja
    ninja install
    ```
    
### Gollvm for compiling Hyperledger Fabric smart contracts


- Run the following instructions to install `gollvm`.

  ``` sh
  export LLVMDIR=$HOME/llvm                   # path to LLVM directory

  mkdir -p $LLVMDIR/src
  cd $LLVMDIR/src
  git clone https://github.com/llvm/llvm-project.git llvm-project-gollvm

  export LLVMGOLLVM=$LLVMDIR/src/llvm-project-gollvm
  cd $LLVMGOLLVM/llvm/tools
  git clone https://go.googlesource.com/gollvm

  cd $LLVMGOLLVM/llvm/tools/gollvm
  git clone https://go.googlesource.com/gofrontend

  cd $LLVMGOLLVM/llvm/tools/gollvm/libgo
  git clone https://github.com/libffi/libffi.git
  git clone https://github.com/ianlancetaylor/libbacktrace.git
  ```
- Checkout the following commits for LLVM-13-compatible version:

  ``` sh
  # Git revisions for working with LLVM 13
  # LLVM: f6b09e394a5fad4b33f8746195377f4f638e2c8d (tag: llvmorg-13.0.0-rc3)
  # gollvm: 0f0479aa582cfa3bd9c17bd7d41d2e2bc9991958
  # gofrontend: e3bfc0889237a5bb8aa7ae30e1cff14f90a5f941
  # libbacktrace: d0f5e95a87a4d3e0a1ed6c069b5dae7cbab3ed2a
  # libffi: 0f2dd369cd5edcefad29b3fca4e1d08cb34f8f19

  # update LLVM to 11.1.0-rc3
  cd $LLVMGOLLVM
  git checkout llvmorg-13.0.0-rc3

  cd $LLVMGOLLVM/llvm/tools/gollvm
  git checkout 0f0479aa582cfa3bd9c17bd7d41d2e2bc9991958

  cd $LLVMGOLLVM/llvm/tools/gollvm/gofrontend
  git checkout e3bfc0889237a5bb8aa7ae30e1cff14f90a5f941

  cd $LLVMGOLLVM/llvm/tools/gollvm/libgo/libbacktrace
  git checkout d0f5e95a87a4d3e0a1ed6c069b5dae7cbab3ed2a
  
  cd $LLVMGOLLVM/llvm/tools/gollvm/libgo/libffi
  git checkout 0f2dd369cd5edcefad29b3fca4e1d08cb34f8f19

  ```

- Compiling Gollvm

  ``` sh
  mkdir -p $LLVMDIR/gollvm
  export GOLLVMDIR=$LLVMDIR/gollvm

  mkdir -p $LLVMGOLLVM/build
  cd $LLVMGOLLVM/build

  # IMPORTANT: make sure to use clang and clang++ version 11-13
  CC=clang CXX=clang++ \
           cmake ../llvm -DCMAKE_INSTALL_PREFIX=$GOLLVMDIR \
           -DLLVM_ENABLE_BINDINGS=OFF -DLLVM_ENABLE_RTTI=ON \
           -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=gold -G Ninja

  ninja gollvm
  ninja install # or ninja install-gollvm
  ```

  After that, the gollvm compiler is installed to `$GOLLVMDIR/bin`.

- Capture LLVM bitcode generated by Gollvm: see `README.md` of
  `llvm-project/llvm/tools/gollvm`.
  
### Build OCaml bindings for LLVM
- Again, from the `build` directory, run cmake and build
  ``` sh
  cmake ../llvm -DLLVM_ENABLE_BINDINGS=ON
  ninja ocaml_doc
  ninja install
  ```

### OCaml for development

- Currently, we are using 4.12.0, or newer.

  ``` sh
  sudo apt-get install opam
  opam init
  opam switch create 4.12.0
  eval $(opam env)
  opam install core unix str ocamlgraph fileutils yaml ezjsonm \
               llvm llvm.target llvm.bitreader llvm.bitwriter llvm.irreader
  ```

## Compilation

- Download source code:

  ``` sh
  export WORKDIR=$HOME/workspace           # or any other working directory
  cd $WORKDIR
  git clone https://github.com/sbip-sg/discover-analyzer
  git clone https://github.com/sbip-sg/llvm-normalizer
  ```

- Update environmental variables:
  ```
  PATH=/path-to-llvm-dir/bin:$PATH
  LD_LIBRARY_PATH=/path-to-llvm-dir/lib:/path-to-llvm-dir/lib64:$LD_LIBRARY_PATH
  ```
  
- Compile `discover`:

  ``` sh
  cd $WORKDIR/discover-analyzer
  make
  ```

- Compile `llvm-normalizer`:

  ``` sh
  cd $WORKDIR/llvm-normalizer
  mkdir -p build; cd build; cmake ..
  make
  ```

- After that, copy the file `llvm-normalizer/build/normalizer` to the folder
  `discover-analyzer`:

  ``` sh
  cp $WORKDIR/llvm-normalizer/build/normalizer $WORKDIR/discover-analyzer/
  ```

# Running Discover

- Run pointer analysis on C program:

  ``` sh
  cd $WORKDIR/discover-analyzer
  ./discover --clang-option "-I ./lib/discover" --dfa-pointer --dfa-inter \
             examples/c/field-read.c
  ```

# Contributing to our Discover project

## Current contributors

- [Ta Quang Trung](https://github.com/taquangtrung/)
- [Ren Kunpeng](https://github.com/kunpengren)
- [Trinh Ngoc Khanh](https://github.com/tnkhanh)
- [Huang Lung-Chen](https://github.com/lung21)

## Want to contribute to our project?

- Please create issues or pull requests to help us improve our tool! :)
