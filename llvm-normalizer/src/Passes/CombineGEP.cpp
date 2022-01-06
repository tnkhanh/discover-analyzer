/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/CombineGEP.h"

using namespace discover;
using namespace llvm;

using GEPInstList = std::vector<GetElementPtrInst *>;

/*
 * This pass combines a sequence of GEP instructions that serve to compute
 * the memory address of only 1 element
 *
 * For example, given the following GEP instructions
 *     u = getelementptr v, idx1, idx2
 *     x = getelementptr u, 0, idx3, idx4
 *     y = getelementptr x, 0, idx5, idx6
 *
 * If u, x are used only once, then the above instructions can be removed,
 * and can be* replaced by a new instruction:
 *     y = getelementptr v, idx1, idx2, idx3, idx4, idx5, idx6
 */

char CombineGEP::ID = 0;

static cl::opt<bool>
    DisableCombineGEP("disable-combine-gep",
                      cl::desc("Disable combining GEP instructions"),
                      cl::init(false), cl::cat(DiscoverNormalizerCategory));

/*
 * Combine GEP Instructions
 */
void combineGEPInstructions(Function &F,
                            std::vector<GEPInstList> combinationGEPList) {
  for (auto it = combinationGEPList.begin(); it != combinationGEPList.end();
       it++) {
    GEPInstList gepInstList = *it;

    auto it2 = gepInstList.begin();

    GetElementPtrInst *firstGEP = *it2;

    std::vector<Value *> newIdxs;
    for (int i = 1; i < firstGEP->getNumOperands(); i++)
      newIdxs.push_back(firstGEP->getOperand(i));

    it2++;
    GetElementPtrInst *otherGEP;
    for (; it2 != gepInstList.end(); it2++) {
      otherGEP = *it2;
      for (int i = 2; i < otherGEP->getNumOperands(); i++)
        newIdxs.push_back(otherGEP->getOperand(i));
    }

    Value *rootPtr = firstGEP->getPointerOperand();
    Instruction *newGepInstr =
        GetElementPtrInst::CreateInBounds(rootPtr, newIdxs);

    IRBuilder<> builder = IRBuilder(firstGEP);
    builder.SetInsertPoint(otherGEP);
    builder.Insert(newGepInstr);
    llvm::replaceOperand(&F, otherGEP, newGepInstr);

    for (it2 = gepInstList.begin(); it2 != gepInstList.end(); it2++) {
      GetElementPtrInst *instr = *it2;
      instr->removeFromParent();
      instr->deleteValue();
    }
  }
}

/*
 * Find GEP Instructions combinable with another instruction
 */
GEPInstList findCombinableGEPs(GetElementPtrInst *instr) {
  // stop finding if the current instr has more than 1 uses
  if (instr->getNumUses() != 1)
    return GEPInstList();

  // Get first user of the gepInstr
  auto it = instr->users().begin();
  User *firstUser = *it;

  // ignore if the current GEP is not used by another GEP
  if (!isa<GetElementPtrInst>(firstUser))
    return GEPInstList();

  GetElementPtrInst *nextInstr = dyn_cast<GetElementPtrInst>(firstUser);
  Value *firstIdx = instr->getOperand(1);

  if (ConstantInt *intIdx = dyn_cast<ConstantInt>(firstIdx)) {
    if (intIdx->getZExtValue() == 0) {
      // Find further combinable GEPInstr
      GEPInstList foundGEPs = findCombinableGEPs(nextInstr);
      foundGEPs.push_back(instr);
      return foundGEPs;
    }
  }

  return GEPInstList();
}

/*
 * Find all GEP instructions that can be combinable
 */
std::vector<GEPInstList> findCombinableGEPList(Function &F) {
  std::vector<GEPInstList> candidateGEPList;
  std::set<GetElementPtrInst *> visitedGEPInstrs;

  BasicBlockList &BS = F.getBasicBlockList();

  for (BasicBlock &B : BS) {
    for (Instruction &I : B) {
      if (!isa<GetElementPtrInst>(&I))
        continue;

      GetElementPtrInst *gepInstr = dyn_cast<GetElementPtrInst>(&I);

      if (visitedGEPInstrs.find(gepInstr) != visitedGEPInstrs.end()) {
        GEPInstList gepInstList = findCombinableGEPs(gepInstr);
        std::reverse(gepInstList.begin(), gepInstList.end());

        for (auto it = gepInstList.begin(); it != gepInstList.end(); it++) {
          visitedGEPInstrs.insert(*it);
        }

        candidateGEPList.push_back(gepInstList);
      }
    }
  }

  return candidateGEPList;
}

/*
 * Entry function for this FunctionPass, can be used by llvm-opt
 */
bool CombineGEP::runOnFunction(Function &F) {
  if (DisableCombineGEP)
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Function Pass <" << passName << "> on: " << F.getName()
          << "\n";

  std::vector<GEPInstList> allGEPList = findCombinableGEPList(F);
  combineGEPInstructions(F, allGEPList);

  debug() << "Finish Function Pass: " << passName << "\n";

  return true;
}

static RegisterPass<CombineGEP> X("CombineGEP", "CombineGEP Pass",
                                  false /* Only looks at CFG */,
                                  false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new CombineGEP());
                                });
