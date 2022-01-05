Discover -- a source code static analyzer
========================================================

Copyright (c) 2020-2022 Singapore Blockchain Innovation Program.

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

# Documentation

- [Homepage](https://www.comp.nus.edu.sg/~dbsystem/discover/).

- [Full documentation](https://discover.readthedocs.io/en/latest/#).

- [Installation guide](INSTALL.md).

# Getting Started

## Compiling Discover

Discover is developed using the OCaml programming language and is built on top
of the LLVM compiler infrastructure. Currently, it can be compiled and works
well in Ubuntu-based Linux environment (Ubuntu, Linux Mint). We have also
successfully compiled and ran it in Arch Linux, macOS, while it hasn't been
tested in Windows yet.

Please see our detailed tutorial at [INSTALL.md](INSTALL.md) for step-by-step guidelines
on how to compile Discover in Ubuntu, Linux Mint.

## Running Discover

IMPORTANT NOTE: this tutorial is a work-in-progress. We will update more
information soon to show how to run Discover to find bugs.

- Example of checking bugs using integer interval analysis (range analysis):

  + Input file: `integer-multiplication.c` (this file is also stored at
    `examples/bugs/c/integer-multiplication.c`):

    ``` c
    #include <stdio.h>
    #include <discover.h>

    int main(int argc, char** argv) {
      int a = 1;

      printf("Input an integer: ");
      scanf("%d", &a);

      // There are potential integer overflow/underflow bugs in the below line
      int x = a * 4;

      // There are potential integer overflow/underflow bugs in the below line
      long y = a * 10;

      long b = a;
      // There is no integer overflow/underflow bug in the below line
      long z = b * 10;

      printf("x: %d\n", x);
      printf("y: %lu\n", y);
      return 0;
    }
    ```

  + Command to run Discover:

    ``` sh
    cd discover-analyzer

    ./discover --clang-option "-I ./lib/discover/ -g" --dfa-range --bug-all \
               examples/bugs/c/integer-multiplication.c
    ```

  + Sample output:

    ``` sh
    BUG: INTEGER OVERFLOW
      Instruction: %v27 = mul nsw i32 %v26, 4, !dbg !24
      Location: file: examples/bugs/c/integer-multiplication.c, 11:13 ~> 11:13
         9.
        10.    // There are potential integer overflow/underflow bugs in the below line
        11.>   int x = a * 4;
           >            ^^^
        12.
        13.    // There are potential integer overflow/underflow bugs in the below line


    BUG: INTEGER OVERFLOW
      Instruction: %v29 = mul nsw i32 %v28, 10, !dbg !27
      Location: file: examples/bugs/c/integer-multiplication.c, 14:14 ~> 14:14
        12.
        13.    // There are potential integer overflow/underflow bugs in the below line
        14.>   long y = a * 10;
           >             ^^^
        15.
        16.    long b = a;


    BUG: INTEGER UNDERFLOW
      Instruction: %v27 = mul nsw i32 %v26, 4, !dbg !24
      Location: file: examples/bugs/c/integer-multiplication.c, 11:13 ~> 11:13
         9.
        10.    // There are potential integer overflow/underflow bugs in the below line
        11.>   int x = a * 4;
           >            ^^^
        12.
        13.    // There are potential integer overflow/underflow bugs in the below line


    BUG: INTEGER UNDERFLOW
      Instruction: %v29 = mul nsw i32 %v28, 10, !dbg !27
      Location: file: examples/bugs/c/integer-multiplication.c, 14:14 ~> 14:14
        12.
        13.    // There are potential integer overflow/underflow bugs in the below line
        14.>   long y = a * 10;
           >             ^^^
        15.
        16.    long b = a;


    ==============================
    Bug Summary:

      Integer Underflow: 2
      Integer Overflow: 2
    ```

# Contributing to Discover

- Discover is developed using OCaml and LLVM. Please see our [development
  guide](docs/development.md) for some detailed information about the development environment,
  coding convention, etc.

- We maintain two active branches in this GitHub repositories:
  + The [`develop`](https://github.com/sbip-sg/discover-analyzer/tree/develop) branch: is updated regularly along with the current
    development.
  + The [`master`](https://github.com/sbip-sg/discover-analyzer/tree/master) branch: is updated occasionally with stable features.

- If you want to be contribute to the development of Discover, please consider
  create issues or pull requests to help us improve our tool! :)
