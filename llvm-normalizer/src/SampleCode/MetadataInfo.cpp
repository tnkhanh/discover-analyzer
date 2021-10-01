
// Print variable names from Metadata
// References:
//  - https://lists.llvm.org/pipermail/llvm-dev/2011-October/044296.html
//  - https://stackoverflow.com/a/21488105

void printVariableOriginalName(Function &F) {
  outs() << "Function: " << F.getName() << "\n";
  BasicBlockList &BS = F.getBasicBlockList();

  for (BasicBlock &B : BS) {
    outs() << " " << B.getName() << "\n";
    for (Instruction &I: B) {
      outs() << "  " << I << "\n";

      if (DbgDeclareInst *dbgDeclare = dyn_cast<DbgDeclareInst>(&I)) {
        DIVariable* var = dbgDeclare->getVariable();
        outs() << "      var name: " << var->getName() << "\n";
      }
      else if (DbgValueInst *dbgValue = dyn_cast<DbgValueInst>(&I)) {
        DIVariable* var = dbgValue->getVariable();
        outs() << "      var name: " << var->getName() << "\n";
      }
    }
    outs() << "\n";
  }

  outs() << "\n\n";
}
