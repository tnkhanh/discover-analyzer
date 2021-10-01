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

using ASLInstrs = std::tuple<AllocaInst*, StoreInst*, std::vector<LoadInst*>>;

struct ElimAllocaStoreArg : public FunctionPass {
  static char ID;

  ElimAllocaStoreArg() : FunctionPass(ID) {}
  virtual bool runOnFunction(Function &F) override;

private:

  void removeAllocaStoreArg(Function&, std::vector<ASLInstrs>);
  std::vector<ASLInstrs> findAllocaStoreArg(Function&);

};

} // namespace discover
