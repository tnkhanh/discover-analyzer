/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/UninlineInstruction.h"

using namespace discover;
using namespace llvm;

/*
 * This pass uninline ConstantExprs, which are operands of an instruction
 * into separate instructions.
 */

char UninlineInstruction::ID = 0;

static cl::opt<bool>
    DisableUninlineInstr("disable-uninline-instr",
                         cl::desc("Disable uninlining instructions"),
                         cl::init(false), cl::cat(DiscoverNormalizerCategory));

/*
 * Un-inline ConstExpr in instructions, recursively
 */
void uninlineConstantExpr(IRBuilder<> *builder, Instruction *instr) {

  // transform ConstantExpr in operands into new instructions
  for (int i = 0; i < instr->getNumOperands(); i++) {
    Value *operand = instr->getOperand(i);
    if (ConstantExpr *expr = dyn_cast<ConstantExpr>(operand)) {
      // outs() << "ConstantExpr: " << *expr << "\n";

      Instruction *exprInstr = expr->getAsInstruction();
      // outs() << "ConstantExprInstr: " << *exprInstr << "\n";

      // debug() << " ConstantExpr: " << *exprInstr << "\n";

      if (PHINode *phiInstr = dyn_cast<PHINode>(instr)) {
        // set the insertion point in the incoming blocks to make sure
        // the current PHINode is always at the beginning of this block
        BasicBlock *incomingBlock = phiInstr->getIncomingBlock(i);
        Instruction *terminator = incomingBlock->getTerminator();
        builder->SetInsertPoint(terminator);
      } else {
        // set the insertion point in this block
        builder->SetInsertPoint(instr);
      }

      builder->Insert(exprInstr);
      instr->setOperand(i, exprInstr);

      uninlineConstantExpr(builder, exprInstr);
    }
  }
}

/*
 * Entry function for this FunctionPass, can be used by llvm-opt
 */
bool UninlineInstruction::runOnFunction(Function &F) {
  if (DisableUninlineInstr)
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Function Pass <" << passName << "> on: " << F.getName()
          << "\n";

  for (BasicBlock &B : F.getBasicBlockList()) {
    IRBuilder<> builder(&B);

    for (Instruction &I : B) {
      uninlineConstantExpr(&builder, &I);
    }
  }

  debug() << "Finish Function Pass: " << passName << "\n";

  return true;
}

static RegisterPass<UninlineInstruction> X("UninlineInstruction",
                                           "UninlineInstruction",
                                           false /* Only looks at CFG */,
                                           false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new UninlineInstruction());
                                });
