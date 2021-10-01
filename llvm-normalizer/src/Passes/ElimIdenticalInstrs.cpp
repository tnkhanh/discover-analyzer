/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/ElimIdenticalInstrs.h"

using namespace discover;
using namespace llvm;

using InstList = std::vector<Instruction*>;
using InstSet = std::set<Instruction*>;
using IdentInsts = std::pair<Instruction*, std::vector<Instruction*>>;
using IdentInstsList = std::vector<IdentInsts>;

/*
 * This pass eliminates identical instructions. For example,
 *
 * Given the following GEP instructions
 *     u = getelementptr v, idx1, idx2
 *     w = getelementptr v, idx1, idx2
 *  ==> replace `w` by `u` and remove the second instruction
 *
 * Given the following PHI instructions
 *     u = phi [v1, b1], [v2, b2]
 *     w = phi [v1, b1], [v2, b2]
 *  ==> replace `w` by `u` and remove the second instruction
 *
 * Given the following BitCast instructions
 *     u = bitcast v to t
 *     w = bitcast v to t
 *  ==> replace `w` by `u` and remove the second instruction
 */

char ElimIdenticalInstrs::ID = 0;

/*
 * Find GetElementPtrInst of the same element pointer
 */
IdentInstsList findGEPOfSameElemPtr(Function &F) {
  InstSet visitedInsts;
  IdentInstsList identInstsList;

  for (BasicBlock &B: F.getBasicBlockList()) {
    for (Instruction &I: B) {
      if (!isa<GetElementPtrInst>(I))
        continue;

      if (visitedInsts.find(&I) != visitedInsts.end())
        continue;

      GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(&I);

      int numIdxs = gepInst->getNumIndices();
      Value *gepSrc = gepInst->getOperand(0);
      if (isa<UndefValue>(gepSrc) || isa<GlobalValue>(gepSrc))
        continue;

      InstList otherIdentInsts;

      for (User *user: gepSrc->users()) {
        if (!isa<GetElementPtrInst>(user) || user == gepInst)
          continue;

        GetElementPtrInst *otherGep = dyn_cast<GetElementPtrInst>(user);
        if ((otherGep->getOperand(0) != gepSrc) ||
            (otherGep->getNumIndices() != numIdxs))
          continue;

        bool hasSameIdxs = true;
        for (int i = 0; i < numIdxs; i++)
          if (otherGep->getOperand(i+1) != gepInst->getOperand(i+1)) {
            hasSameIdxs = false;
            break;
          }

        if (hasSameIdxs) {
          otherIdentInsts.push_back(otherGep);
          visitedInsts.insert(otherGep);
        }
      }

      IdentInsts identInsts = std::make_pair(gepInst, otherIdentInsts);
      identInstsList.push_back(identInsts);
    }
  }

  return identInstsList;
}

/*
 * Find PHINode of the same incoming values
 */
IdentInstsList findPHINodeOfSameIncoming(Function &F) {
  InstSet visitedInsts;
  IdentInstsList identInstsList;

  for (BasicBlock &B: F.getBasicBlockList()) {
    for (Instruction &I: B) {
      if (!isa<PHINode>(I))
        continue;

      if (visitedInsts.find(&I) != visitedInsts.end())
        continue;

      PHINode *phiNode = dyn_cast<PHINode>(&I);

      int numIncoming = phiNode->getNumIncomingValues();
      Value* firstIncoming = phiNode->getIncomingValue(0);
      if (isa<UndefValue>(firstIncoming) || isa<GlobalValue>(firstIncoming))
        continue;

      InstList otherIdentInsts;
      for (User *user: firstIncoming->users()) {
        if (!isa<PHINode>(user) || user == phiNode)
          continue;

        PHINode *otherPhi = dyn_cast<PHINode>(user);
        if (otherPhi->getNumIncomingValues() != numIncoming)
          continue;

        bool hasSameIncomings = true;
        for (int i = 0; i < numIncoming; i++)
          if (phiNode->getIncomingValue(i) != otherPhi->getIncomingValue(i) ||
              phiNode->getIncomingBlock(i) != otherPhi->getIncomingBlock(i)) {
            hasSameIncomings = false;
            break;
          }

        if (hasSameIncomings) {
          otherIdentInsts.push_back(otherPhi);
          visitedInsts.insert(otherPhi);
        }
      }

      IdentInsts identInsts = std::make_pair(phiNode, otherIdentInsts);
      identInstsList.push_back(identInsts);
    }
  }

  return identInstsList;
}


/*
 * Find CastInst of the same source and destination type
 */
IdentInstsList findCastInstsOfSameSourceAndType(Function &F) {
  InstSet visitedInsts;
  IdentInstsList identInstsList;

  for (BasicBlock &B: F.getBasicBlockList()) {
    for (Instruction &I: B) {
      if (!isa<CastInst>(I))
        continue;

      if (visitedInsts.find(&I) != visitedInsts.end())
        continue;

      CastInst *castInst = dyn_cast<CastInst>(&I);
      Value *castSrc = castInst->getOperand(0);

      if (isa<UndefValue>(castSrc) || isa<GlobalValue>(castSrc))
        continue;

      InstList otherIdentInsts;
      for (User *user: castSrc->users()) {
        if (!isa<CastInst>(user) || user == castInst)
          continue;

        CastInst *otherCastInst = dyn_cast<CastInst>(user);

        if (castInst->getDestTy() == otherCastInst->getDestTy()) {
          otherIdentInsts.push_back(otherCastInst);
          visitedInsts.insert(otherCastInst);
        }
      }

      IdentInsts identInsts = std::make_pair(castInst, otherIdentInsts);
      identInstsList.push_back(identInsts);
    }
  }

  return identInstsList;
}

/*
 * Eliminate identical instructions
 */
void eliminateIdenticalInstrs(Function &F, DominatorTree &DT, IdentInstsList identInstsList) {
  for (auto it = identInstsList.begin(); it != identInstsList.end(); it++) {
    IdentInsts identInsts = *it;
    Instruction *keepInst = identInsts.first;
    InstList otherInsts = identInsts.second;

    for (auto it2 = otherInsts.begin(); it2 != otherInsts.end(); it2++) {
      Instruction *otherInst = *it2;
      if (DT.dominates(keepInst, otherInst)) {
        debug() << " replace: " << *otherInst
                << " in " << otherInst->getFunction()->getName() << "\n"
                << "      by: " << *keepInst
                << " in " << keepInst->getFunction()->getName() << "\n";
        llvm::replaceOperand(&F, otherInst, keepInst);
        otherInst->removeFromParent();
        otherInst->deleteValue();
      }
    }
  }
}

/*
 * Entry function for this FunctionPass, can be used by llvm-opt
 */
bool ElimIdenticalInstrs::runOnFunction(Function &F) {
  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Function Pass <" << passName << "> on: "
          << F.getName() << "\n";

  DominatorTree &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();

  // find and eliminate identical CastInst
  IdentInstsList identCastList = findCastInstsOfSameSourceAndType(F);
  eliminateIdenticalInstrs(F, DT, identCastList);

  // find and eliminate identical PHINode
  IdentInstsList identPHIList = findPHINodeOfSameIncoming(F);
  eliminateIdenticalInstrs(F, DT, identPHIList);

  // find and eliminate identical GetElementPtrInst
  IdentInstsList identGEPList = findGEPOfSameElemPtr(F);
  eliminateIdenticalInstrs(F, DT, identGEPList);

  debug() << "Finish Function Pass: " << passName << "\n";

  return true;
}

static RegisterPass<ElimIdenticalInstrs> X("ElimIdenticalInstrs",
    "ElimIdenticalInstrs",
    false /* Only looks at CFG */,
    true /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
    [](const PassManagerBuilder &Builder, legacy::PassManagerBase &PM) {
      PM.add(new ElimIdenticalInstrs());
    });
