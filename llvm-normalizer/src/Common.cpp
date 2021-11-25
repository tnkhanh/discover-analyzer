/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Common.h"
#include "Debug/Debug.h"

using namespace llvm;

// Use an OptionCategory to store all the flags of this tool
cl::OptionCategory llvm::DiscoverNormalizerCategory(
    "LLVM Discover Normalizer Options",
    "Options for the LLVM-normalizer tool of the project Discover.");


bool llvm::isDiscoverTestingFunc(Function &F) {
  StringRef fName = F.getName();

  if (fName.contains("__assert") || fName.contains("__retute"))
    return true;

  return false;
}

void llvm::replaceOperand(Function *func, Value *replacee, Value *replacer) {
  BasicBlockList &blockList = func->getBasicBlockList();

  for (auto it = blockList.begin(); it != blockList.end(); ++it) {
    BasicBlock *blk = &(*it);

    for (auto it2 = blk->begin(); it2 != blk->end(); ++it2) {
      Instruction *instr = &(*it2);

      for (int i = 0; i < instr->getNumOperands(); i++) {
        Value *operand = instr->getOperand(i);
        if (operand == replacee)
          instr->setOperand(i, replacer);
      }
    }
  }
}

void llvm::replacePhiSource(Function *func, BasicBlock *replacee,
                            BasicBlock *replacer) {
  BasicBlockList &blockList = func->getBasicBlockList();

  for (auto it = blockList.begin(); it != blockList.end(); ++it) {
    BasicBlock *blk = &(*it);

    for (auto it2 = blk->begin(); it2 != blk->end(); ++it2) {
      Instruction *instr = &(*it2);

      if (PHINode *phiInstr = dyn_cast<PHINode>(instr)) {
        int numIncomming = phiInstr->getNumIncomingValues();
        for (int i = 0; i < numIncomming; i++) {
          if (phiInstr->getIncomingBlock(i) == replacee)
            phiInstr->setIncomingBlock(i, replacer);
        }
      }
    }
  }
}
