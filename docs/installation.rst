Installation
================

Prerequisites
---------------

To build Discover, you need to have LLVM and OCaml installed in your machine.
Currently, we support the compilation of Discover Linux and macOS, while we have
not tested on Windows yet.

To run Discover with Solidity and Hyperledger Fabric smart contracts, Solang and
Gollvm are also needed.

The following commands are tested and work well with both on Linux and macOS.

LLVM and Clang
^^^^^^^^^^^^^^^^

We use a customized version `LLVM-SBIP
<https://github.com/sbip-sg/llvm-project>`_ of LLVM 13. The latest development
is at the branch ``sbip-llvm-13``. It need to be installed to
``$HOME/llvm/llvm-sbip``. Note that it might take 2 to 3 hours to finish the
compilation of LLVM.

.. code-block:: sh

   # Prepare installation folder
   export LLVMDIR=$HOME/llvm           # root path to LLVM workspace
   export LLVMINSTALLDIR=$HOME/llvm/llvm-sbip
   mkdir -p $LLVMINSTALLDIR

   # Prepare source code
   export LLVMSRCDIR=$HOME/llvm/src
   mkdir -p $LLVMSRCDIR
   cd $LLVMSRCDIR
   git clone https://github.com/sbip-sg/llvm-project llvm-project-sbip
   export LLVMPROJECT=$LLVMSRCDIR/llvm-project-sbip
   cd $LLVMPROJECT
   git checkout sbip-llvm-13

   # Prepare installation directory
   mkdir -p $LLVMPROJECT/build
   cd $LLVMPROJECT/build

   # Configure compilation by CMake.
   # For Linux
   cmake ../llvm -DCMAKE_INSTALL_PREFIX=$LLVMINSTALLDIR \
         -DLLVM_ENABLE_BINDINGS=ON -DLLVM_ENABLE_RTTI=ON \
         -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
         -DLLVM_USE_LINKER=gold -Wno-dev -G Ninja
   # For macOS
   cmake ../llvm -DCMAKE_INSTALL_PREFIX=$LLVMINSTALLDIR \
         -DLLVM_ENABLE_BINDINGS=ON -DLLVM_ENABLE_RTTI=ON \
         -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
         -Wno-dev -G Ninja

   # Build LLVM
   ninja
   ninja ocaml_doc

   # Install LLVM
   ninja install

To update your environment with this LLVM 13 installation:

.. code-block:: sh

   # Option 1: run these commands to update the environment temporarily
   export PATH=$LLVMINSTALLDIR/bin:$PATH
   export LIBRARY_PATH=$LLVMINSTALLDIR/lib:$LIBRARY_PATH
   export LD_LIBRARY_PATH=$LLVMINSTALLDIR/lib:$LD_LIBRARY_PATH
   export DYLD_LIBRARY_PATH=$LLVMINSTALLDIR/lib:$DYLD_LIBRARY_PATH  # for macOS

   # Option 2: put the following to ~/.profile (Linux) or ~/.zshenv (macOS)
   #           to configure the environment permanently
   export LLVMINSTALLDIR=$HOME/llvm/llvm-sbip
   export PATH=$LLVMINSTALLDIR/bin:$PATH
   export LIBRARY_PATH=$LLVMINSTALLDIR/lib:$LIBRARY_PATH
   export LD_LIBRARY_PATH=$LLVMINSTALLDIR/lib:$LD_LIBRARY_PATH
   export DYLD_LIBRARY_PATH=$LLVMINSTALLDIR/lib:$DYLD_LIBRARY_PATH  # for macOS

.. _section-ocaml:

OCaml
^^^^^^

OCaml and its packages can be easily installed via the OCaml Package Management
tool (opam). Currently, we are using OCaml 4.13.1, or newer.

.. code-block:: sh

   # Install Opam (OCaml package manager)
   sudo apt-get install opam
   opam init

   # Install OCaml
   opam switch create 4.13.1
   eval $(opam env)

   # Install dependencies of this project
   opam pin add outils git://github.com/sbip-sg/ocaml-utils.git
   opam install . --deps-only --with-test

Note that ``llvm`` bindings for OCaml will be installed by our `customized LLVM
<https://github.com/sbip-sg/llvm-project>`_ as in the above section.

Solang
^^^^^^^

First, we need to install `the pre-built LLVM
<https://solang.readthedocs.io/en/latest/installing.html#installing-the-llvm-libraries>`_
for Solang (remember to update $PATH with new LLVM and Clang).

Then, there are two methods to install Solang:

- Build and install Solang from source code (recommended for the newest
  version of Solang)

  .. code-block:: sh

     git clone https://github.com/hyperledger-labs/solang solang
     cd solang
     cargo build --release

- Build and install Solang from Cargo repository

  .. code-block:: sh

     cargo install solang

Gollvm
^^^^^^

To install Gollvm, LLVM must be compiled and installed from our `customized LLVM
<https://github.com/sbip-sg/llvm-project>`_. Please see the above Section `LLVM
and Clang`_ to install LLVM first.

Then, run the following instructions to install ``gollvm``. Note that
``$LLVMPROJECT`` and ``$LLVMINSTALLDIR`` are defined as in Section `LLVM
and Clang`_:

.. code-block:: sh

   # Download source code and libraries of gollvm
   cd $LLVMPROJECT/llvm/tools
   git clone https://go.googlesource.com/gollvm
   cd $LLVMPROJECT/llvm/tools/gollvm
   git clone https://go.googlesource.com/gofrontend
   cd $LLVMPROJECT/llvm/tools/gollvm/libgo
   git clone https://github.com/libffi/libffi.git
   git clone https://github.com/ianlancetaylor/libbacktrace.git

   # Check out the following commit for LLVM-13 compatible version:
   cd $LLVMPROJECT/llvm/tools/gollvm
   git checkout 0f0479aa582cfa3bd9c17bd7d41d2e2bc9991958
   cd $LLVMPROJECT/llvm/tools/gollvm/gofrontend
   git checkout e3bfc0889237a5bb8aa7ae30e1cff14f90a5f941
   cd $LLVMPROJECT/llvm/tools/gollvm/libgo/libbacktrace
   git checkout d0f5e95a87a4d3e0a1ed6c069b5dae7cbab3ed2a
   cd $LLVMPROJECT/llvm/tools/gollvm/libgo/libffi
   git checkout 0f2dd369cd5edcefad29b3fca4e1d08cb34f8f19

   # Compile and install Gollvm
   cd $LLVMPROJECT/build
   cmake ../llvm -DCMAKE_INSTALL_PREFIX=$LLVMINSTALLDIR \
            -DLLVM_ENABLE_BINDINGS=ON -DLLVM_ENABLE_RTTI=ON \
            -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
            -DLLVM_USE_LINKER=gold -Wno-dev -G Ninja
   ninja gollvm
   ninja install-gollvm

After that, the gollvm compiler is installed to ``$LLVMINSTALLDIR/bin``.


Compiling Discover
--------------------

Firstly, the two steps of installing LLVM and OCaml above are required. Then,
Discover can be compiled by the following commands

.. code-block:: sh

   # Prepare workspace and source code
   export WORKDIR=$HOME/workspace           # or any other working directory
   cd $WORKDIR
   git clone https://github.com/sbip-sg/discover

   # Compile Discover
   cd $WORKDIR/discover
   make

We also need to compile an additional utility tool named ``normalizer`` and copy
it to the root directory of Discover.

.. code-block:: sh

   # Compile auxiliary tool normalizer
   cd $WORKDIR/discover/llvm-normalizer
   mkdir -p build; cd build; cmake ..
   make
   cp build/normalizer $WORKDIR/discover/

After the above steps, both the two binary files ``discover`` and ``normalizer``
are compiled and copied to the folder ``$WORKDIR/discover/``. Now, users can use
``discover`` to find bugs in their programs or smart contracts.
