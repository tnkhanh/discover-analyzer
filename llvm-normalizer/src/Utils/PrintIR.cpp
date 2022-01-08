/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Utils/PrintIR.h"

using namespace discover;
using namespace llvm;

char PrintIR::ID = 0;

void printFunc(Function &F) {
  outs() << "Function: " << F.getName() << "\n";
  BasicBlockList &BS = F.getBasicBlockList();

  for (BasicBlock &B : BS) {
    outs() << " " << B.getName() << "\n";
    for (Instruction &I : B) {
      outs() << "  " << I << "\n";

      if (DbgDeclareInst *dbgDeclare = dyn_cast<DbgDeclareInst>(&I)) {
        Value *dclValue = dbgDeclare->getAddress();
        outs() << "      Declare value: " << *dclValue << "\n";
        DIVariable *var = dbgDeclare->getVariable();
        outs() << "      Declare name: " << var->getName() << "\n";
      } else if (DbgValueInst *dbgValue = dyn_cast<DbgValueInst>(&I)) {
        Value *varValue = dbgValue->getValue();
        outs() << "      Var value: " << *varValue << "\n";
        DIVariable *var = dbgValue->getVariable();
        outs() << "      Var name: " << var->getName() << "\n";
      }
    }
    outs() << "\n";
  }

  outs() << "\n\n";
}

bool PrintIR::runOnModule(Module &M) {
  for (Function &F : M.getFunctionList())
    printFunc(F);

  return true;
}

static RegisterPass<PrintIR> X("PrintIR", "Print LLVM IR",
                               false /* Only looks at CFG */,
                               false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new PrintIR());
                                });
