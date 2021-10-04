Discover -- a source code static analyzer
========================================================

Copyright (c) 2020-2021 Singapore Blockchain Innovation Program.

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

- Currently, Discover is recommended to run in Linux environment (preferably
  Ubuntu, Linux Mint). We have also successfully compiled and ran it in macOS,
  while Window hasn't been tested yet.

- To compile Discover, please see our detailed tutorial at [INSTALL.md](INSTALL.md).

# Running Discover

IMPORTANT NOTE: this tutorial is a work-in-progress. We will update more
information soon to show how Discover can help to find bugs in programs and
smart contract.

- Run pointer analysis on C program:

  ``` sh
  cd $WORKDIR/discover-analyzer
  ./discover --clang-option "-I ./lib/discover" --dfa-pointer --dfa-inter \
             examples/c/field-read.c
  ```

# Contributing to Discover

## Current contributors

- [Ta Quang Trung](https://github.com/taquangtrung/)
- [Ren Kunpeng](https://github.com/kunpengren)
- [Trinh Ngoc Khanh](https://github.com/tnkhanh)
- [Huang Lung-Chen](https://github.com/lung21)

## Want to contribute to our project?

- Please create issues or pull requests to help us improve our tool! :)
