Installation
================

Solang compiler for Solidity smart contracts
------------------------------------------------

- Full tutorial: https://solang.readthedocs.io/en/latest/installing.html

- Main steps:

  + Install `the pre-built LLVM
    <https://solang.readthedocs.io/en/latest/installing.html#installing-the-llvm-libraries>`_
    for for Solang.

    Remember to update $PATH with new LLVM and Clang.

  + Build and install Solang from source code

    .. code-block:: sh

       git clone https://github.com/hyperledger-labs/solang solang
       cd solang
       cargo build --release

  + Build and install Solang from Cargo repository

    .. code-block:: sh

       cargo install solang
