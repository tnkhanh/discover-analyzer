/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#ifndef COMMON_H
#define COMMON_H

#include <iostream>

#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/LegacyPassManager.h"

#include "llvm/ADT/SetVector.h"


using namespace std;
using namespace llvm;

namespace llvm {

using GlobalVariableList = SymbolTableList<GlobalVariable>;
using FunctionList = SymbolTableList<Function>;
using BasicBlockList = SymbolTableList<BasicBlock>;
using FunctionSet = SmallSetVector<Function*, 16>;

const std::string LLVM_GLOBAL_CTORS = "llvm.global_ctors";

// Use an OptionCategory to store all the flags of this tool
extern cl::OptionCategory DiscoverNormalizerCategory;
extern cl::opt<bool> Debugging;
extern cl::opt<bool> PrintInputProgram;
extern cl::opt<bool> PrintOutputProgram;
extern cl::opt<bool> RunPassesManually;

// Debugging functions
raw_ostream &debug();
raw_ostream &error();

// LLVM utility functions
bool isDiscoverTestingFunc(Function&);
void replaceOperand(Function *func, Value *oldOpr, Value *newOpr);
void replacePhiSource(Function *func, BasicBlock *oldOpr, BasicBlock *newOpr);

}

#endif
