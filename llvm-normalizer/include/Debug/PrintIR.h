/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include <iostream>

#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/GlobalValue.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include "Debug/Debug.h"
#include "Common.h"


using namespace std;
using namespace llvm;

namespace discover {

struct PrintIR : public ModulePass {
  static char ID;

  PrintIR() : ModulePass(ID) {}

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    ModulePass::getAnalysisUsage(AU);
  }

  virtual bool runOnModule(Module &M) override;
};

} // namespace discover
