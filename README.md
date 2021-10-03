Discover -- a source code static analyzer
========================================================

Copyright (c) 2020-2021 Singapore Blockchain Innovation Program.


# Compilation and Installation

- See details at [INSTALL.md](INSTALL.md).

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
