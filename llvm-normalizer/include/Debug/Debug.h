/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#ifndef DEBUG_H
#define DEBUG_H

#include <iostream>
#include <stdarg.h>

#include "llvm/IR/Type.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {

extern bool debugging;
extern bool printInputProgram;
extern bool printOutputProgram;

raw_ostream &debug();
raw_ostream &error();

}

#endif
