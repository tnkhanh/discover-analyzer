Discover -- Compilation and Installation
=======================================================

Copyright (c) 2020-2021 Singapore Blockchain Innovation Program.

# Prerequisites

To build Discover, you need to have LLVM and OCaml installed in your machine.
Currently, we support the compilation of Discover Linux and macOS, while we have
not tested on Windows yet.

The following commands are tested and work well with Linux Mint / Ubuntu 20.

## Tools and libraries

- Ninja-build, z3

  ``` sh
  sudo apt-get install ninja-build, z3
  ```

- LLVM and Clang 13

  + Install from Linux repositories:

    ``` sh
    IMPORTANT: to be updated when LLVM 13 is officially released
    # Ubuntu, Linux Mint
    # sudo apt-get install llvm-13 llvm-13-dev clang-13 libclang-13-dev

    # Arch Linux, Manjaro
    # sudo pacman -S llvm13 clang13
    ```

  + Or, if LLVM and Clang cannot be installed automatically, then download
    prebuilt LLVM and Clang from the [LLVM GitHub Releases](https://github.com/llvm/llvm-project-13/releases), and extract it
    to `$HOME/llvm/llvm-13` (or any other custom directory):

    ``` sh
    # TODO: Revise the followings when LLVM 13 released.
    # Assume that LLVM + Clang 13 binaries are stored at $HOME/llvm/llvm-13/
    # export PATH=$HOME/llvm/llvm-13/bin:$PATH
    # export LD_LIBRARY_PATH=$HOME/llvm/llvm-13/lib:$LD_LIBRARY_PATH
    ```

  + Otherwise, LLVM and Clang 13 can be built from the branch `sbip-llvm-13` of
    our custom [LLVM 13](https://github.com/sbip-sg/llvm-project), and install to `$HOME/llvm/llvm-13`. Note that it
    might take 2 to 3 hours to finish the compilation of LLVM.

    ``` sh
    # Prepare installation folder
    export LLVMDIR=$HOME/llvm           # root path to LLVM workspace
    export LLVMINSTALLDIR=$LLVMDIR/llvm-13
    mkdir -p $LLVMINSTALLDIR

    # Prepare source code
    export LLVMSRCDIR=$LLVMDIR/src
    mkdir -p $LLVMSRCDIR
    cd $LLVMSRCDIR
    git clone https://github.com/sbip-sg/llvm-project llvm-project-sbip
    export $LLVMPROJECT=$LLVMSRCDIR/llvm-project-sbip
    cd $LLVMPROJECT
    git checkout sbip-llvm-13

    # Configure compilation
    mkdir -p $LLVMPROJECT/build
    cd $LLVMPROJECT/build
    cmake ../llvm -DCMAKE_INSTALL_PREFIX=$LLVMINSTALLDIR \
          -DLLVM_ENABLE_BINDINGS=ON -DLLVM_ENABLE_RTTI=ON \
          -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
          -DLLVM_USE_LINKER=gold -Wno-dev -G Ninja

    # Build LLVM
    ninja
    ninja ocaml_doc

    # Install LLVM
    ninja install

    # Update environment
    export PATH=$LLVMINSTALLDIR/bin:$PATH
    export LD_LIBRARY_PATH=$LLVMINSTALLDIR/lib:$LD_LIBRARY_PATH
    ```

## Gollvm for compiling Hyperledger Fabric smart contracts

- To install Gollvm, LLVM must be compiled and installed from [our custom LLVM
  project](https://github.com/sbip-sg/llvm-project). Please follow the above step to install LLVM first.

- Then, run the following instructions to install `gollvm`.

  ``` sh
  # Download source code and libraries of gollvm
  cd $LLVMPROJECT/llvm/tools
  git clone https://go.googlesource.com/gollvm

  cd $LLVMPROJECT/llvm/tools/gollvm
  git clone https://go.googlesource.com/gofrontend

  cd $LLVMPROJECT/llvm/tools/gollvm/libgo
  git clone https://github.com/libffi/libffi.git
  git clone https://github.com/ianlancetaylor/libbacktrace.git
  ```
- Checkout the following commit for LLVM-13 compatible version:

  ``` sh
  cd $LLVMPROJECT/llvm/tools/gollvm
  git checkout 0f0479aa582cfa3bd9c17bd7d41d2e2bc9991958

  cd $LLVMPROJECT/llvm/tools/gollvm/gofrontend
  git checkout e3bfc0889237a5bb8aa7ae30e1cff14f90a5f941

  cd $LLVMPROJECT/llvm/tools/gollvm/libgo/libbacktrace
  git checkout d0f5e95a87a4d3e0a1ed6c069b5dae7cbab3ed2a

  cd $LLVMPROJECT/llvm/tools/gollvm/libgo/libffi
  git checkout 0f2dd369cd5edcefad29b3fca4e1d08cb34f8f19
  ```

- Compiling Gollvm

  ``` sh
  cd $LLVMPROJECT/build
  cmake ../llvm -DCMAKE_INSTALL_PREFIX=$LLVMINSTALLDIR \
           -DLLVM_ENABLE_BINDINGS=ON -DLLVM_ENABLE_RTTI=ON \
           -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
           -DLLVM_USE_LINKER=gold -Wno-dev -G Ninja

  # Compile and install Gollvm
  ninja gollvm
  ninja install-gollvm
  ```

  After that, the gollvm compiler is installed to `$LLVMINSTALLDIR/bin`.

## OCaml for development

- Currently, we are using 4.12.0, or newer.

  ``` sh
  sudo apt-get install opam
  opam init
  opam switch create 4.12.0
  eval $(opam env)
  opam install core unix str ocamlgraph fileutils yaml ezjsonm \
               llvm llvm.target llvm.bitreader llvm.bitwriter llvm.irreader
  ```

- Manually configure LLVM 13 bindings to the opam install directory:

  ``` sh
  opam uninstall llvm
  cd $HOME/.opam/4.12.0/lib
  mkdir -p llvm/static
  cp ocaml/llvm/* llvm/static
  cp ocaml/llvm/* llvm
  ```

# Compilation

- Download source code:

  ``` sh
  export WORKDIR=$HOME/workspace           # or any other working directory
  cd $WORKDIR
  git clone https://github.com/sbip-sg/discover-analyzer
  git clone https://github.com/sbip-sg/llvm-normalizer
  ```

- Update environmental variables, if you build LLVM-13 from source:

  ``` sh
  PATH=$HOME/llvm/llvm-13/bin:$PATH
  LD_LIBRARY_PATH=$HOME/llvm/llvm-13/lib:$HOME/llvm/llvm-13/lib64:$LD_LIBRARY_PATH
  export LIBRARY_PATH=$HOME/llvm/llvm-13/lib:$HOME/llvm/llvm-13/lib64:$LIBRARY_PATH
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
