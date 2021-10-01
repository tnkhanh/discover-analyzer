/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Debug/PrintIR.h"

using namespace discover;
using namespace llvm;

char PrintIR::ID = 0;

void printFunc(Function &F) {
  outs() << "Function: " << F.getName() << "\n";
  BasicBlockList &BS = F.getBasicBlockList();

  for (BasicBlock &B : BS) {
    outs() << " " << B.getName() << "\n";
    for (Instruction &I: B) {
      outs() << "  " << I << "\n";

      if (CallInst *callInst = dyn_cast<CallInst>(&I)) {
        Function *callee = callInst->getCalledFunction();
        if (callee->getName().equals("llvm.dbg.value")) {
          outs() << "      Callee: " << callee->getName() << "\n";
          outs() << "       num args: " << callInst->getNumArgOperands() << "\n";
          Value* arg0 = callInst->getArgOperand(0);
          ValueName* vname = arg0->getValueName();
          outs() << "       is metadata? " << arg0->getType()->isMetadataTy() << "\n";
          outs() << "       arg0: " << arg0->getValueName() << "\n";
          outs() << "       arg0: " << arg0->getName() << "\n";
        }
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


static RegisterPass<PrintIR> X("PrintIR",
                               "Print LLVM IR",
                               false /* Only looks at CFG */,
                               false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new PrintIR());
                                });
