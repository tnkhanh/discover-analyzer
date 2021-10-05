Discover -- a source code static analyzer
========================================================

Copyright (c) 2020-2021 Singapore Blockchain Innovation Program.

Homepage: https://www.comp.nus.edu.sg/~dbsystem/discover/

# Overview

Discover is a static analysis tool that aims find bugs and vulnerabilities in
computer programs and smart contracts. Currently, it can analyze programs
written in C, C++, or any languages that can be compiled directly to LLVM
bitcode. We are also working to extend Discover to analyze smart contracts
written in Golang, Typescript of Hyperledger Fabric blockchain and in Solidity
of Ethereum blockchain.

At its core, Discover is equipped (or will be equipped) with state-of-the-art
static analysis techniques to find different bug types:

- Data-flow analysis for integer bugs such as integer overflows, integer
  underflow, conversion error, division-by-zero (in C, C++, Solidity, Golang,
  Typescript) and out-of-gas bug (in Solidity).

- Control flow analysis for smart contract bugs such as reentrancy in Solidity.

- Separation logic and pointer analysis for memory bugs such as null-pointer
  dereference, memory leaks (in C/C++), buffer overflow (in C, C++, Solidity,
  Golang, Typescript).

To handle large program (of thousands of lines of code), we also develop a
sparse analysis framework based on data-flow analysis.

# Compiling Discover

Discover is developed using the OCaml programming language and is built on top
of the LLVM compiler infrastructure. Currently, it can be compiled and works
well in Ubuntu-based Linux environment (Ubuntu, Linux Mint). We have also
successfully compiled and ran it in Arch Linux, macOS, while it hasn't been
tested in Windows yet.

Please see our detailed tutorial at [INSTALL.md](INSTALL.md) for step-by-step guidelines
on how to compile Discover in Ubuntu, Linux Mint.

# Running Discover

IMPORTANT NOTE: this tutorial is a work-in-progress. We will update more
information soon to show how to run Discover to find bugs.

- Example of checking bugs using integer interval analysis (range analysis):

  ``` sh
  cd $WORKDIR/discover-analyzer
  ./discover --clang-option "-I ./lib/discover/ -g" --dfa-range --bug-all \
             examples/bugs/c/integer-overflow-annot.c
  ```

  Sample output:

  ```sh
  BUG: INTEGER OVERFLOW
  Instruction: %v27 = mul nsw i32 %v26, 4, !dbg !24
  Location: file: examples/bugs/c/integer-overflow-annot.c, 11:38 ~> 11:38
     9.
    10.    // BUG: there is an integer overflow bug in the below line
    11.>   int x = /*{Bug:IntegerOverflow*/ a * 4 /*:Bug}*/;
       >                                     ^^^
    12.


  BUG: INTEGER OVERFLOW
    Instruction: %v29 = mul nsw i32 %v28, 10, !dbg !27
    Location: file: examples/bugs/c/integer-overflow-annot.c, 14:39 ~> 14:39
      12.
      13.    // BUG: there is an integer overflow bug in the below line
      14.>   long y = /*{Bug:IntegerOverflow*/ a * 10 /*:Bug}*/;
         >                                      ^^^
      15.
  ```

# Contributing to Discover

## Current contributors

- [Ta Quang Trung](https://github.com/taquangtrung/)
- [Ren Kunpeng](https://github.com/kunpengren)
- [Trinh Ngoc Khanh](https://github.com/tnkhanh)
- [Huang Lung-Chen](https://github.com/lung21)

## Want to contribute to our project?

- Please create issues or pull requests to help us improve our tool! :)
