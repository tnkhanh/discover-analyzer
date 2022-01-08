/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/ElimInlineAsm.h"

using namespace discover;
using namespace llvm;

char ElimInlineAsm::ID = 0;

// command line option
static cl::opt<bool>
    DisableElimInlineAsm("disable-elim-inline-asm",
                         cl::desc("Disable elmininate inline asm instructions"),
                         cl::init(false), cl::cat(DiscoverNormalizerCategory));

// command line option
static cl::opt<bool>
    EnableElimInlineAsm("enable-elim-inline-asm",
                        cl::desc("Enable elmininate inline asm instructions"),
                        cl::init(false), cl::cat(DiscoverNormalizerCategory));

bool ElimInlineAsm::runOnModule(Module &M) {
  if (DisableElimInlineAsm || (RunPassesManually && !EnableElimInlineAsm))
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Module Pass: " << passName << "\n";

  string inlineAsm = M.getModuleInlineAsm();
  int checksumPos = inlineAsm.find("\n\t.ascii \"checksum");

  if (checksumPos != string::npos) {
    int nextLinePos = inlineAsm.find('\n', checksumPos + 1);
    const string expectedLastLineToDelete = "\n\t.text\n";
    if (nextLinePos != string::npos &&
        inlineAsm.substr(nextLinePos, 8) == expectedLastLineToDelete) {
      M.setModuleInlineAsm(inlineAsm.substr(nextLinePos + 8));

      debug() << "Final module-level inline asm: __" << M.getModuleInlineAsm()
              << "__\n";
    } else
      debug() << "Module-level inline asm is unchanged\n";
  } else
    debug() << "Module-level inline asm is unchanged\n";

  debug() << "Finish Module Pass: " << passName << "\n";

  return true;
}

static RegisterPass<ElimInlineAsm> X("ElimInlineAsm", "ElimInlineAsm",
                                     false /* Only looks at CFG */,
                                     true /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new ElimInlineAsm());
                                });
