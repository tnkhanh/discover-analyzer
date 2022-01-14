/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/ElimAllocaStoreArg.h"

using namespace discover;
using namespace llvm;

/*
 * This pass eliminates a sequence of  Alloca, Load, Store instructions
 * that only serves as temporary data holder of function arguments:
 *
 * For example:
 *     u = alloca int*
 *     store arg, u
 *     x = load b
 *
 * Then the above instructions can be removed, and every appearance of `x`
 * can be replaced by `arg`
 */

char ElimAllocaStoreArg::ID = 0;

// command line option
static cl::opt<bool> DisableElimAllocaStoreArg(
    "disable-elim-alloca-store-arg",
    cl::desc("Disable elmininate alloca instructions storing arguments"),
    cl::init(false), cl::cat(DiscoverNormalizerCategory));

// command line option
static cl::opt<bool> EnableElimAllocaStoreArg(
    "enable-elim-alloca-store-arg",
    cl::desc("Enable elmininate alloca instructions storing arguments"),
    cl::init(false), cl::cat(DiscoverNormalizerCategory));

void ElimAllocaStoreArg::removeAllocaStoreArg(Function &F,
                                              std::vector<ASLInstrs> ASLList) {
  for (auto it = ASLList.begin(); it != ASLList.end(); it++) {
    ASLInstrs instrTuple = *it;

    AllocaInst *allocInst = std::get<0>(instrTuple);
    StoreInst *storeInst = std::get<1>(instrTuple);
    std::vector<LoadInst *> loadInsts = std::get<2>(instrTuple);

    debug() << "- Elim AllocaInst: " << *allocInst << "\n";
    debug() << "  StoreInst: " << *storeInst << "\n";

    Value *storeSrc = storeInst->getOperand(0);

    for (auto it = loadInsts.begin(); it != loadInsts.end(); it++) {
      LoadInst *loadInst = *it;
      debug() << "   replace: " << *loadInst << "\n"
              << "      by: " << *storeSrc << "\n";
      llvm::replaceOperand(&F, loadInst, storeSrc);
      // loadInst->removeFromParent();
      // loadInst->deleteValue();
      loadInst->eraseFromParent();
    }

    // storeInst->removeFromParent();
    // storeInst->deleteValue();
    storeInst->eraseFromParent();

    // allocInst->removeFromParent();
    // allocInst->deleteValue();
    allocInst->eraseFromParent();

    // debug() << "Output function:\n" << F;
  }
}

std::vector<ASLInstrs> ElimAllocaStoreArg::findAllocaStoreArg(Function &F) {
  std::vector<ASLInstrs> candidateAllocaStoreArgList;

  BasicBlockList &BS = F.getBasicBlockList();

  for (BasicBlock &B : BS) {
    for (Instruction &I : B) {
      if (!isa<AllocaInst>(&I))
        continue;

      AllocaInst *allocInst = dyn_cast<AllocaInst>(&I);
      // debug() << "AllocaInst: " << *allocInst << "\n";

      StoreInst *storeInst;
      std::vector<LoadInst *> loadInsts;

      bool isStoreLoadArgOnly = true;
      int numStoreInst = 0;
      int numLoadInst = 0;

      for (auto it = allocInst->user_begin(); it != allocInst->user_end();
           it++) {
        Value *allocUser = *it;
        // debug() << "  user: " << *it->getUser() << "\n";

        if (storeInst = dyn_cast<StoreInst>(allocUser)) {
          if (!isa<Argument>(storeInst->getOperand(0)) ||
              storeInst->getOperand(1) != allocInst)
            isStoreLoadArgOnly = false;
          // debug() << "  StoreInst: " << *storeInst << "\n";
          numStoreInst++;
        } else if (LoadInst *loadInst = dyn_cast<LoadInst>(allocUser)) {
          // debug() << "  LoadInst: " << *loadInst << "\n";
          loadInsts.push_back(loadInst);
          numLoadInst++;
        } else {
          isStoreLoadArgOnly = false;
          break;
        }
      }

      if (isStoreLoadArgOnly && numStoreInst == 1 && numLoadInst > 0) {
        ASLInstrs candidate = std::make_tuple(allocInst, storeInst, loadInsts);
        candidateAllocaStoreArgList.push_back(candidate);
      }
    }
  }

  return candidateAllocaStoreArgList;
}

bool ElimAllocaStoreArg::runOnFunction(Function &F) {
  if (DisableElimAllocaStoreArg ||
      (RunPassesManually && !EnableElimAllocaStoreArg))
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Function Pass <" << passName << "> on: " << F.getName()
          << "\n";

  // if (F.getName().equals("parse_shell_options"))
  //   debug() << "Input function: " << F << "\n";

  std::vector<ASLInstrs> instrTupleList = findAllocaStoreArg(F);
  removeAllocaStoreArg(F, instrTupleList);

  debug() << "Finish Function Pass: " << passName << "\n";

  // if (F.getName().equals("parse_shell_options"))
  //   debug() << "Output function: " << F << "\n";

  return true;
}

static RegisterPass<ElimAllocaStoreArg> X("ElimAllocaStoreArg",
                                          "ElimAllocaStoreArg",
                                          false /* Only looks at CFG */,
                                          false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new ElimAllocaStoreArg());
                                });
