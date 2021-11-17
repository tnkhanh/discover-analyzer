Discover
==========

Overview
--------

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

Getting Started
----------------


Contents
==========

.. toctree::
   :maxdepth: 2
   :caption: Development

   development.rst
