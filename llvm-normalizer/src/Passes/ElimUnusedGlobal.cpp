/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/ElimUnusedGlobal.h"

using namespace discover;
using namespace llvm;

char ElimUnusedGlobal::ID = 0;

static cl::opt<bool> DisableElimUnusedGlobal(
    "disable-elim-unused-global", cl::desc("Disable elmininate unused globals"),
    cl::init(false), cl::cat(DiscoverNormalizerCategory));

bool ElimUnusedGlobal::runOnModule(Module &M) {
  if (DisableElimUnusedGlobal)
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Module Pass: " << passName << "\n";

  GlobalVariableList &globalList = M.getGlobalList();
  SmallSetVector<GlobalVariable *, 16> removableGlobals;

  for (GlobalVariable &global : globalList)
    if (global.getNumUses() == 0)
      removableGlobals.insert(&global);

  for (GlobalVariable *global : removableGlobals) {
    debug() << " - Deleting " << *global << "\n";
    global->removeFromParent();
    // global->deleteValue();
  }

  debug() << "Finish Module Pass: " << passName << "\n";

  return true;
}

static RegisterPass<ElimUnusedGlobal> X("ElimUnusedGlobal", "ElimUnusedGlobal",
                                        false /* Only looks at CFG */,
                                        true /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new ElimUnusedGlobal());
                                });
