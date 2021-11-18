/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

// Print variable names from Metadata
// References:
//  - https://lists.llvm.org/pipermail/llvm-dev/2011-October/044296.html
//  - https://stackoverflow.com/a/21488105

void printVariableOriginalName(Function &F) {
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
