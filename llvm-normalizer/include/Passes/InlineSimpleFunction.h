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
#include "llvm/IR/AbstractCallSite.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Dominators.h"

#include "llvm/Analysis/InlineCost.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include "Debug/Debug.h"
#include "Common.h"

using namespace std;
using namespace llvm;

namespace discover {

struct InlineSimpleFunction : public ModulePass {
  static char ID;
  static bool inlineFunction(Module &M, vector<string> funcNames);

  void setInlineableFunctions(Module &M);
  Function* findCandidate(Module &M, FunctionSet visited);
  bool inlineFunction(Module &M, Function* func);

  InlineSimpleFunction() : ModulePass(ID) {}

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    ModulePass::getAnalysisUsage(AU);
    // AU.addRequired<DominatorTreeWrapperPass>();
  }

  virtual bool runOnModule(Module &M) override;
};

} // namespace discover
