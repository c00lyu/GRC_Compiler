/*
 * CGGRCRuntime.cpp
 *
 *  Created on: Jul 31, 2014
 *      Author: jiechaolou
 */
#include "CGGRCRuntime.h"
#include "CGCall.h"
#include "CodeGenFunction.h"
#include "clang/AST/Decl.h"
#include "clang/AST/ExprCXX.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include <iostream>


using namespace clang;
using namespace CodeGen;
using namespace llvm;

llvm::Constant *CGGRCRuntime::getGrcSetupPEAArgumentFn() const{
	  std::vector<llvm::Type*> Params;
	  Params.push_back(VoidPtrTy);
	  Params.push_back(SizeTy);
	  Params.push_back(SizeTy);
	  return CGM.CreateRuntimeFunction(llvm::FunctionType::get(IntTy,
	                                                           Params, false),
	                                   "__gr_SetupPEAArgument");
}

llvm::Constant *CGGRCRuntime::getGrcCallPEAFn() const{
	  std::vector<llvm::Type*> Params;
	  Params.push_back(CharPtrTy);
	  return CGM.CreateRuntimeFunction(llvm::FunctionType::get(IntTy,
	                                                           Params, false),
	                                   "__gr_CallPEA");
}


void CGGRCRuntime::EmitPEAHead(CodeGenFunction &CGF){
	  int GrcParallelNum = CGF.getGrcParallelNum();
	  llvm::Module &M = CGF.CGM.getModule();
	  SmallVector<llvm::Value *, 16> GrcSharedVars = CGF.getGrcSharedVarList();
	  std::vector<llvm::Type *> GrcSharedTypes;
	  for (SmallVector<llvm::Value *, 16>::iterator I = GrcSharedVars.begin(), E = GrcSharedVars.end();
	       I != E; ++I) {
	    llvm::Value *V = *I;
	    assert(isa<llvm::PointerType>(V->getType()) && "Arg type not PointerType");
	    GrcSharedTypes.push_back(cast<llvm::PointerType>(V->getType())->getElementType());
	  }
	  llvm::StructType *VarStackTy = llvm::StructType::get(
	      CGF.getLLVMContext(), GrcSharedTypes);


	  llvm::BasicBlock *EndBlock = CGF.createBasicBlock("setup.end");

	  // Emit the calls to cudaSetupArgument
	  llvm::Constant *GrcSetupPEAArgFn = getGrcSetupPEAArgumentFn();
	  for (unsigned I = 0, E = GrcSharedVars.size(); I != E; ++I) {
	    llvm::Value *Vars[3];
	    llvm::BasicBlock *NextBlock = CGF.createBasicBlock("setup.next");
	    Vars[0] = CGF.Builder.CreatePointerCast(GrcSharedVars[I], VoidPtrTy);
	    Vars[1] = CGF.Builder.CreateIntCast(
	        llvm::ConstantExpr::getSizeOf(GrcSharedTypes[I]),
	        SizeTy, false);
	    Vars[2] = CGF.Builder.CreateIntCast(
	        llvm::ConstantExpr::getOffsetOf(VarStackTy, I),
	        SizeTy, false);
	    llvm::CallSite CS = CGF.EmitRuntimeCallOrInvoke(GrcSetupPEAArgFn, Vars);
	    llvm::Constant *Zero = llvm::ConstantInt::get(IntTy, 0);
	    llvm::Value *CSZero = CGF.Builder.CreateICmpEQ(CS.getInstruction(), Zero);
	    CGF.Builder.CreateCondBr(CSZero, NextBlock, EndBlock);
	    CGF.EmitBlock(NextBlock);
	  }
	  CGF.EmitBranch(EndBlock);
	  CGF.EmitBlock(EndBlock);
	  //----------------------------------------------------------------------------------
	  //Set the characteristic of a Global Variable
	  const FunctionDecl *FD = dyn_cast<FunctionDecl>(CGF.CurFuncDecl);
	  std::string Head = ".str.";
	  std::string FnName = FD->getName().data();
	  std::string UndLine = "_";
	  Twine tmpT(GrcParallelNum+1);
	  std::string GPNum = tmpT.str();
	  std::string GVName = Head+FnName+UndLine+GPNum;
	  std::string GVvalue = FnName+UndLine+GPNum+".config";
	  StringRef GVValue(GVvalue);

	  LLVMContext &Context = CGF.CGM.getLLVMContext();
	  llvm::Type *GVConstTy = IntegerType::get(Context,8);
	  unsigned int size = GVValue.size();
	  llvm::Type *Result = llvm::ArrayType::get(GVConstTy, size+1);
	  bool UnnamedAddr = true;
	  bool IsConstant = true;
	  GlobalValue::LinkageTypes Linkage = GlobalValue::PrivateLinkage;
	  GlobalValue::VisibilityTypes Visibility = GlobalValue::DefaultVisibility;
	  bool IsExternallyInitialized = false;
	  GlobalVariable::ThreadLocalMode TLM = GlobalVariable::NotThreadLocal;
	  unsigned Alignment = 1;
	  unsigned AddrSpace = 0;
	  //----------------------------------------------------------------------------------
	  GlobalVariable *GV = new GlobalVariable(M, Result, false,
	      		                    GlobalValue::ExternalLinkage, 0,
	                                GVName, 0, GlobalVariable::NotThreadLocal,
	                                AddrSpace);

	  Constant *GVV = ConstantDataArray::getString(Context, GVValue,
	  	                                                        true);
	  if (GVV)
	    GV->setInitializer(GVV);
	  GV->setConstant(IsConstant);
	  GV->setLinkage((GlobalValue::LinkageTypes)Linkage);
	  GV->setVisibility((GlobalValue::VisibilityTypes)Visibility);
	  GV->setExternallyInitialized(IsExternallyInitialized);
	  GV->setThreadLocalMode(TLM);
	  GV->setUnnamedAddr(UnnamedAddr);
	  GV->setAlignment(Alignment);

	  CGF.addGrcTaskGlobalStringMetadata(GV);
	  //----------------------------------------------------------------------------------
	  //get GetElementPtr
	  unsigned int numBits = ((1 * 64) / 19) + 2;
	  APInt Tmp(numBits, StringRef("0"), 10);
	  APSInt APS = APSInt(Tmp, true);
	  llvm::Type *Ty = IntegerType::get(Context, 32);
	  APS = APS.extOrTrunc(Ty->getPrimitiveSizeInBits());
	  Constant *V = ConstantInt::get(Context, APS);
	  SmallVector <Constant *, 16> Elts;
	  Elts.push_back(GV);
	  Elts.push_back(V);
	  Elts.push_back(V);
	  ArrayRef<Constant *> Indices(Elts.begin() + 1, Elts.end());
	  Constant * Getelement = ConstantExpr::getGetElementPtr(Elts[0], Indices,true);
	  SmallVector<Value*, 8> callPEAArgs;
	  callPEAArgs.push_back(Getelement);

	  // Emit the call to cudaLaunch
	  llvm::Constant *GrcCallPEAFn = getGrcCallPEAFn();
	  CGF.EmitRuntimeCallOrInvoke(GrcCallPEAFn, callPEAArgs);
}

llvm::Constant *CGGRCRuntime::getGrcSetupRPUArgumentFn() const{
	  std::vector<llvm::Type*> Params;
	  Params.push_back(VoidPtrTy);
	  Params.push_back(SizeTy);
	  Params.push_back(SizeTy);
	  return CGM.CreateRuntimeFunction(llvm::FunctionType::get(IntTy,
	                                                           Params, false),
	                                   "__gr_SetupRPUArgument");
}

llvm::Constant *CGGRCRuntime::getGrcCallRPUFn() const{
	  std::vector<llvm::Type*> Params;
	  Params.push_back(CharPtrTy);
	  Params.push_back(IntTy);
	  return CGM.CreateRuntimeFunction(llvm::FunctionType::get(IntTy,
	                                                           Params, false),
	                                   "__gr_CallRPU");
}

void CGGRCRuntime::EmitRPUHead(CodeGenFunction &CGF,const FunctionDecl *FD,
		                       SmallVector<llvm::Value *,16> &Args,
		                       bool Syn){
	  llvm::Module &M = CGF.CGM.getModule();
	  llvm::LLVMContext &Ctx = M.getContext();
	  std::vector<llvm::Type *> ArgsTypes;
	  for (SmallVector<llvm::Value *, 16>::iterator I = Args.begin(), E = Args.end();
	       I != E; ++I) {
	    llvm::Value *V = *I;
	    assert(isa<llvm::PointerType>(V->getType()) && "Arg type not PointerType");
	    ArgsTypes.push_back(cast<llvm::PointerType>(V->getType())->getElementType());
	  }
	  llvm::StructType *VarStackTy = llvm::StructType::get(
	      CGF.getLLVMContext(), ArgsTypes);


	  llvm::BasicBlock *EndBlock = CGF.createBasicBlock("setup.end");

	  // Emit the calls to cudaSetupArgument
	  llvm::Constant *GrcSetupRPUArgFn = getGrcSetupRPUArgumentFn();
	  for (unsigned I = 0, E = Args.size(); I != E; ++I) {
	    llvm::Value *Vars[3];
	    llvm::BasicBlock *NextBlock = CGF.createBasicBlock("setup.next");
	    Vars[0] = CGF.Builder.CreatePointerCast(Args[I], VoidPtrTy);
	    Vars[1] = CGF.Builder.CreateIntCast(
	        llvm::ConstantExpr::getSizeOf(ArgsTypes[I]),
	        SizeTy, false);
	    Vars[2] = CGF.Builder.CreateIntCast(
	        llvm::ConstantExpr::getOffsetOf(VarStackTy, I),
	        SizeTy, false);
	    llvm::CallSite CS = CGF.EmitRuntimeCallOrInvoke(GrcSetupRPUArgFn, Vars);
	    llvm::Constant *Zero = llvm::ConstantInt::get(IntTy, 0);
	    llvm::Value *CSZero = CGF.Builder.CreateICmpEQ(CS.getInstruction(), Zero);
	    CGF.Builder.CreateCondBr(CSZero, NextBlock, EndBlock);
	    CGF.EmitBlock(NextBlock);
	  }
	  CGF.EmitBranch(EndBlock);
	  CGF.EmitBlock(EndBlock);
	  //----------------------------------------------------------------------------------
	  //Set the characteristic of a Global Variable
	  std::string Head = ".str.";
	  std::string FnName = FD->getName().data();
	  std::string GVName = Head+FnName;
	  std::string GVvalue = FnName+".arm";
	  StringRef GVValue(GVvalue);

	  LLVMContext &Context = CGF.CGM.getLLVMContext();
	  llvm::Type *GVConstTy = IntegerType::get(Context,8);
	  unsigned int size = GVValue.size();
	  llvm::Type *Result = llvm::ArrayType::get(GVConstTy, size+1);
	  bool UnnamedAddr = true;
	  bool IsConstant = true;
	  GlobalValue::LinkageTypes Linkage = GlobalValue::PrivateLinkage;
	  GlobalValue::VisibilityTypes Visibility = GlobalValue::DefaultVisibility;
	  bool IsExternallyInitialized = false;
	  GlobalVariable::ThreadLocalMode TLM = GlobalVariable::NotThreadLocal;
	  unsigned Alignment = 1;
	  unsigned AddrSpace = 0;
	  //----------------------------------------------------------------------------------
	  GlobalVariable *GV = new GlobalVariable(M, Result, false,
	      		                    GlobalValue::ExternalLinkage, 0,
	                                GVName, 0, GlobalVariable::NotThreadLocal,
	                                AddrSpace);

	  Constant *GVV = ConstantDataArray::getString(Context, GVValue,
	  	                                                        true);
	  if (GVV)
	    GV->setInitializer(GVV);
	  GV->setConstant(IsConstant);
	  GV->setLinkage((GlobalValue::LinkageTypes)Linkage);
	  GV->setVisibility((GlobalValue::VisibilityTypes)Visibility);
	  GV->setExternallyInitialized(IsExternallyInitialized);
	  GV->setThreadLocalMode(TLM);
	  GV->setUnnamedAddr(UnnamedAddr);
	  GV->setAlignment(Alignment);

	  //----------------------------------------------------------------------------------
	  //get GetElementPtr
	  unsigned int numBits = ((1 * 64) / 19) + 2;
	  APInt Tmp(numBits, StringRef("0"), 10);
	  APSInt APS = APSInt(Tmp, true);
	  llvm::Type *Ty = IntegerType::get(Context, 32);
	  APS = APS.extOrTrunc(Ty->getPrimitiveSizeInBits());
	  Constant *V = ConstantInt::get(Context, APS);
	  SmallVector <Constant *, 16> Elts;
	  Elts.push_back(GV);
	  Elts.push_back(V);
	  Elts.push_back(V);
	  ArrayRef<Constant *> Indices(Elts.begin() + 1, Elts.end());
	  Constant * Getelement = ConstantExpr::getGetElementPtr(Elts[0], Indices,true);
	  SmallVector<Value*, 8> callPEAArgs;
	  callPEAArgs.push_back(Getelement);
	  callPEAArgs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), Syn));

	  // Emit the call to cudaLaunch
	  llvm::Constant *GrcCallRPUFn = getGrcCallRPUFn();
	  CGF.EmitRuntimeCallOrInvoke(GrcCallRPUFn, callPEAArgs);
}

void CGGRCRuntime::EmitCallRPU(CodeGenFunction &CGF,const FunctionDecl *FD,
		           bool Syn){
	  llvm::Module &M = CGF.CGM.getModule();
	  llvm::LLVMContext &Ctx = M.getContext();
	  //----------------------------------------------------------------------------------
	  //Set the characteristic of a Global Variable
	  std::string Head = ".str.";
	  std::string FnName = FD->getName().data();
	  std::string GVName = Head+FnName;
	  std::string GVvalue = FnName+".arm";
	  StringRef GVValue(GVvalue);

	  LLVMContext &Context = CGF.CGM.getLLVMContext();
	  llvm::Type *GVConstTy = IntegerType::get(Context,8);
	  unsigned int size = GVValue.size();
	  llvm::Type *Result = llvm::ArrayType::get(GVConstTy, size+1);
	  bool UnnamedAddr = true;
	  bool IsConstant = true;
	  GlobalValue::LinkageTypes Linkage = GlobalValue::PrivateLinkage;
	  GlobalValue::VisibilityTypes Visibility = GlobalValue::DefaultVisibility;
	  bool IsExternallyInitialized = false;
	  GlobalVariable::ThreadLocalMode TLM = GlobalVariable::NotThreadLocal;
	  unsigned Alignment = 1;
	  unsigned AddrSpace = 0;
	  //----------------------------------------------------------------------------------
	  GlobalVariable *GV = new GlobalVariable(M, Result, false,
	      		                    GlobalValue::ExternalLinkage, 0,
	                                GVName, 0, GlobalVariable::NotThreadLocal,
	                                AddrSpace);

	  Constant *GVV = ConstantDataArray::getString(Context, GVValue,
	  	                                                        true);
	  if (GVV)
	    GV->setInitializer(GVV);
	  GV->setConstant(IsConstant);
	  GV->setLinkage((GlobalValue::LinkageTypes)Linkage);
	  GV->setVisibility((GlobalValue::VisibilityTypes)Visibility);
	  GV->setExternallyInitialized(IsExternallyInitialized);
	  GV->setThreadLocalMode(TLM);
	  GV->setUnnamedAddr(UnnamedAddr);
	  GV->setAlignment(Alignment);

	  //----------------------------------------------------------------------------------
	  //get GetElementPtr
	  unsigned int numBits = ((1 * 64) / 19) + 2;
	  APInt Tmp(numBits, StringRef("0"), 10);
	  APSInt APS = APSInt(Tmp, true);
	  llvm::Type *Ty = IntegerType::get(Context, 32);
	  APS = APS.extOrTrunc(Ty->getPrimitiveSizeInBits());
	  Constant *V = ConstantInt::get(Context, APS);
	  SmallVector <Constant *, 16> Elts;
	  Elts.push_back(GV);
	  Elts.push_back(V);
	  Elts.push_back(V);
	  ArrayRef<Constant *> Indices(Elts.begin() + 1, Elts.end());
	  Constant * Getelement = ConstantExpr::getGetElementPtr(Elts[0], Indices,true);
	  SmallVector<Value*, 8> callPEAArgs;
	  callPEAArgs.push_back(Getelement);
	  callPEAArgs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), Syn));

	  // Emit the call to cudaLaunch
	  llvm::Constant *GrcCallRPUFn = getGrcCallRPUFn();
	  CGF.EmitRuntimeCallOrInvoke(GrcCallRPUFn, callPEAArgs);
}

void CGGRCRuntime::EmitTaskFunBody(CodeGenFunction &CGF,
		                           FunctionArgList &Args){
	  // Build the argument value list and the argument stack struct type.
	  SmallVector<llvm::Value *, 16> ArgValues;
	  std::vector<llvm::Type *> ArgTypes;
	  for (FunctionArgList::const_iterator I = Args.begin(), E = Args.end();
	       I != E; ++I) {
	    llvm::Value *V = CGF.GetAddrOfLocalVar(*I);
	    ArgValues.push_back(V);
	    assert(isa<llvm::PointerType>(V->getType()) && "Arg type not PointerType");
	    ArgTypes.push_back(cast<llvm::PointerType>(V->getType())->getElementType());
	  }
	  llvm::StructType *ArgStackTy = llvm::StructType::get(
	      CGF.getLLVMContext(), ArgTypes);

	  llvm::BasicBlock *EndBlock = CGF.createBasicBlock("setup.end");

	  // Emit the calls to __gr_SetupRPUArgument
	  llvm::Constant *GrcSetupRPUArgFn = getGrcSetupRPUArgumentFn();
	  for (unsigned I = 0, E = Args.size(); I != E; ++I) {
	    llvm::Value *Args[3];
	    llvm::BasicBlock *NextBlock = CGF.createBasicBlock("setup.next");
	    Args[0] = CGF.Builder.CreatePointerCast(ArgValues[I], VoidPtrTy);
	    Args[1] = CGF.Builder.CreateIntCast(
	        llvm::ConstantExpr::getSizeOf(ArgTypes[I]),
	        SizeTy, false);
	    Args[2] = CGF.Builder.CreateIntCast(
	        llvm::ConstantExpr::getOffsetOf(ArgStackTy, I),
	        SizeTy, false);
	    llvm::CallSite CS = CGF.EmitRuntimeCallOrInvoke(GrcSetupRPUArgFn, Args);
	    llvm::Constant *Zero = llvm::ConstantInt::get(IntTy, 0);
	    llvm::Value *CSZero = CGF.Builder.CreateICmpEQ(CS.getInstruction(), Zero);
	    CGF.Builder.CreateCondBr(CSZero, NextBlock, EndBlock);
	    CGF.EmitBlock(NextBlock);
	  }

	  CGF.EmitBranch(EndBlock);
	  CGF.EmitBlock(EndBlock);
}

/// Creates an instance of a GRC runtime class.
CGGRCRuntime *CodeGen::CreateGRCRuntime(CodeGenModule &CGM){
	return new CGGRCRuntime(CGM);
}


