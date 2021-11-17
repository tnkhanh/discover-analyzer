llvm-normalizer
====================
*A LLVM bitcode normalizer to support the Discover analyzer*

# Compiling llvm-Normalizer

## Prerequisites

- LLVM and Clang 12.0

  + Install from Linux repositories:

    ```sh
    # Ubuntu, Linux Mint
    sudo apt-get install llvm-12 llvm-12-dev clang-12 libclang-12-dev

    # Arch Linux, Manjaro
    sudo pacman -S llvm12 clang12
    ```
  + If LLVM and Clang cannot be installed automatically, then in Ubuntu-based
    operating systems, user can download a pre-built LLVM and Clang version 12
    from the [LLVM GitHub Releases](https://github.com/llvm/llvm-project/releases), and extract it to `$HOME/llvm/llvm-12`.

    Then, run the following commands to update the environment variables:

    ``` sh
    # Assume that LLVM + Clang 12 binaries are stored at $HOME/llvm/llvm-12/
    export PATH=$HOME/llvm/llvm-12/bin:$PATH
    export LD_LIBRARY_PATH=$HOME/llvm/llvm-12/lib:$LD_LIBRARY_PATH
    ```

  + Otherwise, LLVM and Clang 12 can be downloaded and built from source code.
    To do this, download source code of LLVM 12 from the [LLVM GitHub
    Releases](https://github.com/llvm/llvm-project/releases) and extract it to `$HOME/llvm/src/llvm-project-12/`:

    ``` sh
    # Assume that LLVM 12 source code is stored at $HOME/llvm/src/llvm-project/
    mkdir -p $HOME/llvm/llvm-12
    cd $HOME/llvm/src/llvm-project/
    mkdir -p build; cd build
    cmake ../llvm -DLLVM_ENABLE_PROJECTS=clang -DCMAKE_BUILD_TYPE=Release \
          -DCMAKE_INSTALL_PREFIX=$HOME/llvm/llvm-12 -Wno-dev -G Ninja
    ninja
    ninja install
    export PATH=$HOME/llvm/llvm-12/bin:$PATH
    export LD_LIBRARY_PATH=$HOME/llvm/llvm-12/lib:$LD_LIBRARY_PATH
    ```

  + Make sure that LLVM and Clang is built with `-frtti` so that llvm-normalizer
    can be compiled correctly. To check that, run

    ```sh
    nm -C </path/to/libclang.so>
    ```

    and search for the appearance of both `vtable` and `typeinfo`. If
    both of them appear, and LLVM and Clang is built with `-frtti`.

- Dependencies of the external library [`backward-cpp`](https://github.com/bombela/backward-cpp) for pretty printing
  back-trace during the development.

  + Quick installation:

    ```sh
    sudo apt install binutils-dev libdw-dev libdwarf-dev

    ```

  + Full tutorial: please refer to the homepage of [`backward-cpp`](https://github.com/bombela/backward-cpp) for
    detailed explanation on the installation of its dependencies.

## Compilation

- Modify the file `CMakeLists.txt` to update `LLVM_CMAKE_PATH_HINTS` to the
  CMake folder corresponding the installed LLVM, (version 12 is preferred).
  For examples:

  ``` cmake
  set(LLVM_CMAKE_PATH_HINTS
  "$HOME/llvm/llvm-12/lib/cmake/llvm"              # manually installed
  "/usr/lib/llvm-12/lib/cmake/llvm/"               # Ubuntu installed
  ...)
  ```

- Make sure the default command `clang` in your system refers Clang version 12.

  + For Ubuntu-based system, if there are multiple installed versions of Clang,
    then use `update-alternatives` to set Clang 12 by default. For example:

    ```sh
    # First time use update-alternatives to set up clang 12:
    sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-12 90
    sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-12 90

    # If Clang 12 was set up before, then update it by:
    sudo update-alternatives --set clang /usr/bin/clang-12
    ```

  + Otherwise, run `source scripts/setup-env-llvm-12`

- Compile `llvm-normalizer`

  ``` sh
  mkdir build
  cd build
  cmake ../
  make
  ```

- After compilation, copy the output file `llvm-normalizer` to the same folder
  with the main tool `discover`.

# Development

- Auto-format code using `clang-format`:

  ```sh
  cd build
  cmake ../
  make clangformat
  ```

# Running llvm-normalizer

- (TODO: write detailed tutorial).
