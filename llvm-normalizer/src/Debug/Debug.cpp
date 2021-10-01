/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Debug/Debug.h"

using namespace llvm;

// initialize external variables
bool llvm::debugging = false;
bool llvm::printInputProgram = false;
bool llvm::printOutputProgram = false;

raw_ostream &llvm::debug() {
  if (debugging)
    return outs();
  else
    return nulls();
}

raw_ostream &llvm::error() {
  if (debugging)
    return errs();
  else
    return nulls();
}
