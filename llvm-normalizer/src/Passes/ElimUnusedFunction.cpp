/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/ElimUnusedFunction.h"

using namespace discover;
using namespace llvm;

char ElimUnusedFunction::ID = 0;

// command line option
static cl::opt<bool>
    DisableElimUnusedFunction("disable-elim-unused-function",
                              cl::desc("Disable elmininate unused function"),
                              cl::init(false),
                              cl::cat(DiscoverNormalizerCategory));

// command line option
static cl::opt<bool>
    EnableElimUnusedFunction("enable-elim-unused-function",
                             cl::desc("Enable elmininate unused function"),
                             cl::init(false),
                             cl::cat(DiscoverNormalizerCategory));

bool ElimUnusedFunction::runOnModule(Module &M) {
  if (DisableElimUnusedFunction ||
      (RunPassesManually && !EnableElimUnusedFunction))
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Module Pass: " << passName << "\n";

  bool continueRemove = true;

  while (continueRemove) {
    continueRemove = false;

    FunctionList &funcList = M.getFunctionList();
    FunctionSet unusedFuncs;

    for (auto it = funcList.begin(); it != funcList.end(); ++it) {
      Function *func = &(*it);

      StringRef funcName = func->getName();

      // do note eliminate assertions or main function
      if (funcName.startswith("__assert") || funcName.startswith("__refute") ||
          funcName.contains("main"))
        continue;

      // do not eliminate internal linkage function
      if (func->hasInternalLinkage())
        continue;

      if (func->getNumUses() == 0)
        unusedFuncs.insert(func);
    }

    for (Function *func : unusedFuncs) {
      debug() << "Eliminating unused function: " << func->getName() << "\n";
      // func->removeFromParent();
      func->eraseFromParent();
      continueRemove = true;
    }
  }

  debug() << "Finish Module Pass: " << passName << "\n";

  return true;
}

bool ElimUnusedFunction::normalizeModule(Module &M) {
  ElimUnusedFunction pass;
  return pass.runOnModule(M);
}

static RegisterPass<ElimUnusedFunction>
    X("elim-unused-funcs",          // flag to run pass
      "Elimimate Unused Functions", // Pass name
      false,                        // Only looks at CFG
      false);                       // Analysis Pass

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new ElimUnusedFunction());
                                });
