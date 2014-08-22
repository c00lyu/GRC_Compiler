/*
 * CGGRCRuntime.h
 *
 *  Created on: Jul 31, 2014
 *      Author: jiechaolou
 */

#ifndef CGGRCRUNTIME_H_
#define CGGRCRUNTIME_H_

#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/AST/Decl.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/CallSite.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>

namespace clang {

class GrcTaskCallExpr;

namespace CodeGen {

class CodeGenFunction;
class CodeGenModule;
class FunctionArgList;
class ReturnValueSlot;
class RValue;

class CGGRCRuntime {
protected:
  CodeGenModule &CGM;
  llvm::Type *IntTy, *SizeTy;
  llvm::PointerType *CharPtrTy, *VoidPtrTy;

public:
  CGGRCRuntime(CodeGenModule &CGM) : CGM(CGM) {
	  CodeGen::CodeGenTypes &Types = CGM.getTypes();
	   ASTContext &Ctx = CGM.getContext();

	   IntTy = Types.ConvertType(Ctx.IntTy);
	   SizeTy = Types.ConvertType(Ctx.getSizeType());

	   CharPtrTy = llvm::PointerType::getUnqual(Types.ConvertType(Ctx.CharTy));
	   VoidPtrTy = cast<llvm::PointerType>(Types.ConvertType(Ctx.VoidPtrTy));
  }

  ~CGGRCRuntime(){}

  llvm::Constant *getGrcSetupPEAArgumentFn() const;
  llvm::Constant *getGrcCallPEAFn() const;
  void EmitPEAHead(CodeGenFunction &CGF);

  llvm::Constant *getGrcCallRPUFn() const;
  llvm::Constant *getGrcSetupRPUArgumentFn() const;
  void EmitRPUHead(CodeGenFunction &CGF,const FunctionDecl *FD,
		           SmallVector<llvm::Value *,16> &Args,bool Syn);
  void EmitCallRPU(CodeGenFunction &CGF,const FunctionDecl *FD,
  		           bool Syn);
  void EmitTaskFunBody(CodeGenFunction &CGF,
		               FunctionArgList &Args);
};

/// Creates an instance of a GRC runtime class.
CGGRCRuntime *CreateGRCRuntime(CodeGenModule &CGM);
}
}
#endif /* CGGRCRUNTIME_H_ */
