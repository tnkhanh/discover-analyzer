/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/InlineSimpleFunction.h"

using namespace discover;
using namespace llvm;

static cl::opt<bool>
    DisableInlineSimpleFunction("disable-inline-simple-function",
                                cl::desc("Disable inlining simple functions"),
                                cl::init(false));

char InlineSimpleFunction::ID = 0;

bool hasGlobalValue(Function &F) {
  for (BasicBlock &B : F)
    for (Instruction &I : B)
      for (Value *Op : I.operands())
        if (GlobalValue *G = dyn_cast<GlobalValue>(Op))
          return true;

  return false;
}

// A function is called a transfer-call function if it only performs
// bitcast, call another function once and return the result;
bool isCallTransferFunc(Function &F) {
  // debug() << "Checking Call-Transfer function: " << F.getName() << "\n";
  BasicBlockList &blockList = F.getBasicBlockList();
  if (blockList.size() == 1) {
    // debug() << " has 1 block\n";
    BasicBlock &B = blockList.front();
    for (Instruction &I : B) {
      if (!(isa<CallInst>(&I)) && !(isa<BitCastInst>(&I)) &&
          !(isa<ReturnInst>(&I)))
        return false;
    }
    return true;
  }
  return false;
}

// A function is called a transfer-call function if it only performs
// bitcast, call another function once and return the result;
bool isGEPTransferFunc(Function &F) {
  // debug() << "Checking GEP-Transfer function: " << F.getName() << "\n";
  BasicBlockList &blockList = F.getBasicBlockList();
  if (blockList.size() == 1) {
    // debug() << " has 1 block\n";
    BasicBlock &B = blockList.front();
    for (Instruction &I : B) {
      if (!(isa<GetElementPtrInst>(&I)) && !(isa<BitCastInst>(&I)) &&
          !(isa<ReturnInst>(&I)))
        return false;
    }
    return true;
  }
  return false;
}

Function *InlineSimpleFunction::findCandidate(Module &M,
                                              FunctionSet attemptedFuncs) {
  FunctionList &funcList = M.getFunctionList();

  for (Function &F : funcList) {
    bool attempted = false;
    for (Function *f : attemptedFuncs) {
      if (f->getName().equals(F.getName())) {
        attempted = true;
        break;
      }
    }

    // debug() << " processed: " << attempted << "\n";

    if (attempted || isDiscoverTestingFunc(F))
      continue;

    if (!(F.hasLinkOnceLinkage()) && !(F.hasLinkOnceODRLinkage()) &&
        !(F.hasInternalLinkage()))
      continue;

    if (isCallTransferFunc(F))
      return &F;

    if (isGEPTransferFunc(F))
      return &F;
  }

  return NULL;
}

bool InlineSimpleFunction::inlineFunction(Module &M, Function *F) {
  debug() << "* Start to inline function: " << F->getName() << "\n";
  StringRef funcName = F->getName();
  llvm::InlineFunctionInfo IFI;

  SmallSetVector<CallBase *, 16> Calls;
  for (User *U : F->users())
    if (auto CB = dyn_cast<CallBase>(U))
      if (CB->getCalledFunction() == F)
        Calls.insert(CB);

  bool successful = true;

  for (CallBase *CB : Calls) {
    InlineResult res = llvm::InlineFunction(*CB, IFI);
    successful = successful && res.isSuccess();
  }

  if (successful) {
    debug() << "    Inline succeeded!\n";
    if (F->getNumUses() == 0) {
      F->eraseFromParent();
      debug() << "    Removed the inlined function from parent!\n";
      return true;
    } else {
      debug() << "    Failed to remove the inlined function from parent!\n";
      return false;
    }
  }

  debug() << "    Inline failed!\n";

  return false;
}

bool InlineSimpleFunction::runOnModule(Module &M) {
  if (DisableInlineSimpleFunction)
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Module Pass: " << passName << "\n";

  FunctionSet attemptedFuncs = FunctionSet();

  while (true) {
    Function *F = findCandidate(M, attemptedFuncs);

    if (!F)
      break;

    if (!inlineFunction(M, F))
      attemptedFuncs.insert(F);
  }

  debug() << "Finish Module Pass: " << passName << "\n";

  return true;
}

bool InlineSimpleFunction::inlineFunction(Module &M, vector<string> funcNames) {

  for (string funcName : funcNames) {

    Function *inlineFunc = NULL;

    for (Function &F : M.getFunctionList()) {
      if (F.getName().equals(funcName)) {
        inlineFunc = &F;
        break;
      }
    }

    if (inlineFunc == NULL) {
      debug() << "Error: function not found: " << funcName << "\n";
      return false;
    }

    InlineSimpleFunction pass;
    pass.inlineFunction(M, inlineFunc);
  }

  return true;
}

static RegisterPass<InlineSimpleFunction> X("InlineSimpleFunction",
                                            "Inline Simple Function",
                                            false /* Only looks at CFG */,
                                            false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new InlineSimpleFunction());
                                });
