/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include <iostream>

#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/CFG.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include "Debug/Debug.h"
#include "Common.h"

using namespace std;
using namespace llvm;

namespace discover {

struct ElimIdenticalInstrs : public FunctionPass {
  static char ID;
  ElimIdenticalInstrs() : FunctionPass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<DominatorTreeWrapperPass>();
  }

  virtual bool runOnFunction(Function &F) override;
};

} // namespace discover
