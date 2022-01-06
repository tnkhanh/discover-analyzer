/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include <iostream>

#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

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

struct InitGlobal : public ModulePass {
  static char ID;

  InitGlobal() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) override;

private:
  void uninlineConstantExpr(IRBuilder<>*, Instruction*);

  void uninlinePointerInitValue(LLVMContext&, IRBuilder<>*,
                                GlobalVariable*, Constant*, PointerType*);

  void uninlineAggregateInitValue(LLVMContext&, IRBuilder<>*, GlobalVariable*,
                                  std::vector<Value*>, Constant*);

  void invokeGlobalInitFunctions(IRBuilder<>*, GlobalVariable*, Constant*);

};
} // namespace discover
