/********************************************************************
 * This file is part of the tool Normalizer of the project Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include "Passes/InitGlobal.h"

using namespace discover;
using namespace llvm;

char InitGlobal::ID = 0;

static cl::opt<bool> DisableInitGlobal("disable-init-global",
                                       cl::desc("Disable initializing globals"),
                                       cl::init(false),
                                       cl::cat(DiscoverNormalizerCategory));

void InitGlobal::uninlineConstantExpr(IRBuilder<> *builder,
                                      Instruction *instr) {
  // transform ConstantExpr in operands into new instructions
  for (int i = 0; i < instr->getNumOperands(); i++) {
    Value *operand = instr->getOperand(i);
    if (ConstantExpr *expr = dyn_cast<ConstantExpr>(operand)) {
      Instruction *exprInstr = expr->getAsInstruction();
      uninlineConstantExpr(builder, exprInstr);
      builder->Insert(exprInstr);
      instr->setOperand(i, exprInstr);
    }
  }
}

void InitGlobal::uninlinePointerInitValue(LLVMContext &ctx,
                                          IRBuilder<> *builder,
                                          GlobalVariable *global,
                                          Constant *initValue,
                                          PointerType *initType) {
  debug() << "++ Processing Pointer Global: " << *global << "\n";
  debug() << "   Init value: " << *initValue << "\n";
  debug() << "   Init value type: " << *(initValue->getType()) << "\n";

  if (initValue->isNullValue())
    return;

  Align align = global->getAlign().getValue();

  // then initialize this field in the global initialization function
  // Instruction* loadInst = new LoadInst(global);
  // builder.Insert(loadInst);
  if (ConstantExpr *exprInit = dyn_cast<ConstantExpr>(initValue)) {
    Instruction *exprInstr = exprInit->getAsInstruction();
    builder->Insert(exprInstr);
    global->setOperand(0, ConstantPointerNull::get(initType));
    // debug() << "      New init value: " << *initValue << "\n";
    Instruction *storeInst = new StoreInst(exprInstr, global, false, align);
    builder->Insert(storeInst);
  }
}

void InitGlobal::uninlineAggregateInitValue(LLVMContext &ctx,
                                            IRBuilder<> *builder,
                                            GlobalVariable *global,
                                            std::vector<Value *> gepIdxs,
                                            Constant *initValue) {
  // debug() << "++ Processing Aggregate Global: " << *global << "\n"
  //         << "   Current indices: ";
  // for (Value* idx: gepIdxs) {
  //   debug() << *idx << ",  ";
  // }
  // debug() << "\n";
  // debug() << "   Init value: " << *initValue << "\n";
  // debug() << "   Init value type: " << *(initValue->getType()) << "\n";

  if (initValue->isNullValue() || initValue->isZeroValue())
    return;

  MaybeAlign mbAlign = global->getAlign();
  if (!mbAlign.hasValue())
    return;

  Align align = mbAlign.getValue();

  // ConstExpr
  if (ConstantExpr *exprInit = dyn_cast<ConstantExpr>(initValue)) {
    // then initialize this field in the global initialization function
    Instruction *exprInstr = exprInit->getAsInstruction();
    uninlineConstantExpr(builder, exprInstr);
    builder->Insert(exprInstr);
    ArrayRef<Value *> idxs = (ArrayRef<Value *>)gepIdxs;
    Instruction *gepInst =
        GetElementPtrInst::CreateInBounds(global, idxs, "gep");
    // debug() << "   New GepInst1: " << *gepInst << "\n";
    builder->Insert(gepInst);
    Instruction *storeInst = new StoreInst(exprInstr, gepInst, false, align);
    builder->Insert(storeInst);
  }

  // Constant Integer
  else if (isa<ConstantInt>(initValue) || isa<Function>(initValue) ||
           isa<GlobalVariable>(initValue)) {
    ArrayRef<Value *> idxs = (ArrayRef<Value *>)gepIdxs;
    Instruction *gepInst =
        GetElementPtrInst::CreateInBounds(global, idxs, "gep");
    // debug() << "   New GepInst2: " << *gepInst << "\n";
    builder->Insert(gepInst);
    Instruction *storeInst = new StoreInst(initValue, gepInst, false, align);
    builder->Insert(storeInst);
  }

  // Constant Struct
  else if (ConstantStruct *structInit = dyn_cast<ConstantStruct>(initValue)) {
    for (int i = 0; i < structInit->getNumOperands(); i++) {
      Constant *fieldInit = structInit->getOperand(i);

      if (fieldInit->isNullValue() || fieldInit->isZeroValue())
        continue;

      Value *fieldIdx = ConstantInt::get(IntegerType::get(ctx, 32), i);
      std::vector<Value *> currentIdxs(gepIdxs);
      currentIdxs.push_back(fieldIdx);

      Type *fieldTyp = fieldInit->getType();

      if (PointerType *ptrTyp = dyn_cast<PointerType>(fieldTyp))
        structInit->setOperand(i, ConstantPointerNull::get(ptrTyp));
      else if (IntegerType *intTyp = dyn_cast<IntegerType>(fieldTyp))
        structInit->setOperand(i, ConstantInt::get(intTyp, 0));

      uninlineAggregateInitValue(ctx, builder, global, currentIdxs, fieldInit);
    }
  }

  // unline a struct
  else if (ConstantArray *arrayInit = dyn_cast<ConstantArray>(initValue)) {
    for (int i = 0; i < arrayInit->getNumOperands(); i++) {
      Constant *elemInit = arrayInit->getOperand(i);

      if (elemInit->isNullValue() || elemInit->isZeroValue())
        continue;

      Value *elemIdx = ConstantInt::get(IntegerType::get(ctx, 32), i);
      std::vector<Value *> currentIdxs(gepIdxs);
      currentIdxs.push_back(elemIdx);

      Type *elemInitTyp = elemInit->getType();

      if (PointerType *ptrTyp = dyn_cast<PointerType>(elemInitTyp))
        arrayInit->setOperand(i, ConstantPointerNull::get(ptrTyp));
      else if (IntegerType *intTyp = dyn_cast<IntegerType>(elemInitTyp))
        arrayInit->setOperand(i, ConstantInt::get(intTyp, 0));

      uninlineAggregateInitValue(ctx, builder, global, currentIdxs, elemInit);
    }
  }
}

/*
 * Init global variables capture in ‘llvm.global_ctors‘
 * See https://releases.llvm.org/6.0.1/docs/LangRef.html
 */
void InitGlobal::invokeGlobalInitFunctions(IRBuilder<> *builder,
                                           GlobalVariable *global,
                                           Constant *initValue) {
  vector<pair<int, Function *>> prioFuncs;

  for (int i = 0; i < initValue->getNumOperands(); i++) {
    Value *operand = initValue->getOperand(i);
    if (ConstantStruct *st = dyn_cast<ConstantStruct>(operand)) {
      if (st->getNumOperands() == 3) {
        pair<int, Function *> prioFunc;
        if (ConstantInt *p = dyn_cast<ConstantInt>(st->getOperand(0)))
          prioFunc.first = p->getValue().getSExtValue();
        if (Function *f = dyn_cast<Function>(st->getOperand(1)))
          prioFunc.second = f;
        prioFuncs.push_back(prioFunc);
      }
    }
  }

  std::sort(prioFuncs.begin(), prioFuncs.end(),
            [](pair<int, Function *> fp1, pair<int, Function *> fp2) {
              return (fp1.first < fp2.first);
            });

  for (pair<int, Function *> fp : prioFuncs) {
    Function *func = fp.second;
    Instruction *callInst = CallInst::Create(func);
    builder->Insert(callInst);
  }

  return;
}

bool InitGlobal::runOnModule(Module &M) {
  if (DisableInitGlobal)
    return true;

  StringRef passName = this->getPassName();
  debug() << "=========================================\n"
          << "Running Module Pass: " << passName << "\n";

  GlobalVariableList &globalList = M.getGlobalList();
  LLVMContext &ctx = M.getContext();

  // create a function to initialize global variables
  Type *retType = Type::getVoidTy(ctx);
  vector<Type *> argTypes;
  FunctionType *funcType = FunctionType::get(retType, argTypes, false);
  Function *funcInit = Function::Create(funcType, Function::ExternalLinkage, 0,
                                        "__init_globals", nullptr);
  funcInit->setDSOLocal(true);

  BasicBlock *blkInit = BasicBlock::Create(ctx, "", funcInit);
  IRBuilder<> builder(blkInit);

  for (auto it = globalList.begin(); it != globalList.end(); ++it) {
    GlobalVariable *global = &(*it);
    // outs() << "Global Var: " << *global << "\n";

    if (!(global->hasInitializer()))
      continue;

    StringRef globalName = global->getName();
    Constant *initValue = global->getInitializer();
    Type *initType = initValue->getType();

    // call global vars constructors
    if (globalName.equals(LLVM_GLOBAL_CTORS)) {
      invokeGlobalInitFunctions(&builder, global, initValue);
    }
    // uninline init values of globals
    else if (initType->isAggregateType()) {
      std::vector<Value *> gepIdxs;
      gepIdxs.push_back(ConstantInt::get(IntegerType::get(ctx, 32), 0));
      uninlineAggregateInitValue(ctx, &builder, global, gepIdxs, initValue);
    } else if (PointerType *pInitType = dyn_cast<PointerType>(initType)) {
      uninlinePointerInitValue(ctx, &builder, global, initValue, pInitType);
    }
  }

  // delete the initialization function when it is not needed
  if (blkInit->size() != 0) {
    Instruction *returnInst = ReturnInst::Create(ctx);
    builder.Insert(returnInst);

    // insert the function to the beginning of the list
    FunctionList &funcList = M.getFunctionList();
    funcList.push_front(funcInit);
  }

  debug() << "Finish Module Pass: " << passName << "\n";

  debug() << "Function init globals: \n" << *funcInit;

  return true;
}

static RegisterPass<InitGlobal> X("InitGlobal", "InitGlobal",
                                  false /* Only looks at CFG */,
                                  false /* Analysis Pass */);

static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new InitGlobal());
                                });
