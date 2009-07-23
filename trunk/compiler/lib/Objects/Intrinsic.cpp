/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Expr.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "tart/Objects/Intrinsic.h"
#include "tart/Objects/Builtins.h"

namespace tart {
  
using namespace llvm;

namespace {

// -------------------------------------------------------------------
// Often intrinsics need to dereference input params
const Expr * derefMacroParam(const Expr * in) {
  if (const LValueExpr * lval = dyn_cast<LValueExpr>(in)) {
    if (lval->value()->defnType() == Defn::Let) {
      const VariableDefn * defn = static_cast<const VariableDefn *>(lval->value());
      if (defn->initValue() != NULL) {
        return defn->initValue();
      }
    }
  }
  
  return in;
}

}

// -------------------------------------------------------------------
// Intrinsic

StringMap<Intrinsic *> Intrinsic::intrinsicMap;

llvm::Value * Intrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DFAIL("IllegalState");
}
  
Intrinsic * Intrinsic::get(const char * name) {
  static bool init = false;
  if (!init) {
    init = true;
  }
  
  llvm::StringMap<Intrinsic *>::const_iterator it = intrinsicMap.find(name);
  if (it != intrinsicMap.end()) {
    return it->second;
  }

  diag.fatal() << "Unknown intrinsic function '" << name << "'";
  return NULL;
}

// -------------------------------------------------------------------
// StringifyIntrinsic
StringifyIntrinsic StringifyIntrinsic::instance;

Value * StringifyIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  std::stringstream sstream;
  FormatStream fs(sstream);

  const Expr * arg = derefMacroParam(call->arg(0));
  fs << arg;

  DASSERT_OBJ(!sstream.str().empty(), arg);
  return cg.genStringLiteral(sstream.str());
}

// -------------------------------------------------------------------
// PrimitiveToStringIntrinsic
PrimitiveToStringIntrinsic PrimitiveToStringIntrinsic::instance;

Value * PrimitiveToStringIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const FunctionDefn * func = call->function();
  const Expr * self = call->getSelfArg();
  const Expr * formatString = call->arg(0);
  
  Value * selfArg = cg.genExpr(self);
  Value * formatStringArg = cg.genExpr(formatString);
  
  const PrimitiveType * ptype = cast<PrimitiveType>(dealias(self->getType()));
  TypeId id = ptype->getTypeId();
  
  if (functions_[id] == NULL) {
    char funcName[32];
    snprintf(funcName, sizeof funcName, "%s_toString", ptype->typeDefn()->name());
    
    const llvm::Type * funcType = func->getType()->getIRType();
    functions_[id] = llvm::Function::Create(cast<llvm::FunctionType>(funcType),
        Function::ExternalLinkage, funcName, cg.irModule());
  }

  return cg.builder().CreateCall2(functions_[id], selfArg, formatStringArg);
}

// -------------------------------------------------------------------
// LocationOfIntrinsic
LocationOfIntrinsic LocationOfIntrinsic::instance;

Value * LocationOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  std::stringstream sstream;

  const Expr * arg = derefMacroParam(call->arg(0));
  SourceLocation loc = arg->getLocation();
  if (loc.file != NULL) {
    TokenPosition pos = loc.file->getTokenPosition(loc);
    sstream << loc.file->getFilePath() << ":" << pos.beginLine + 1 << ":";
  }
  
  return cg.genStringLiteral(sstream.str());
}

// -------------------------------------------------------------------
// VAllocIntrinsic
VAllocIntrinsic VAllocIntrinsic::instance;

Value * VAllocIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Type * retType = dealias(call->getType());
  return cg.genVarSizeAlloc(call->getLocation(), retType, call->arg(0));
}

// -------------------------------------------------------------------
// PVAllocIntrinsic
PVAllocIntrinsic PVAllocIntrinsic::instance;

Value * PVAllocIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Type * retType = dealias(call->getType());
  return cg.genVarSizeAlloc(call->getLocation(), retType, call->arg(0));
}

// -------------------------------------------------------------------
// ZeroPtrIntrinsic
ZeroPtrIntrinsic ZeroPtrIntrinsic::instance;

Value * ZeroPtrIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Type * retType = dealias(call->getType());
  const llvm::Type * type = retType->getIRType();
  return ConstantPointerNull::get(PointerType::getUnqual(type));
}

// -------------------------------------------------------------------
// AddressOfIntrinsic
AddressOfIntrinsic AddressOfIntrinsic::instance;

Value * AddressOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 1);
  Value * argVal = cg.genLValueAddress(call->arg(0));
  return argVal;
}

// -------------------------------------------------------------------
// PointerDiffIntrinsic
PointerDiffIntrinsic PointerDiffIntrinsic::instance;

Value * PointerDiffIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 2);
  const Expr * firstPtr = call->arg(0);
  const Expr * lastPtr = call->arg(1);
  // TODO: Throw an exception if it won't fit...
  // TODO: Should use uintptr_t instead of int32.
  
  DASSERT_OBJ(firstPtr->getType()->isEqual(lastPtr->getType()), call);
  Type * elemType = cast<NativePointerType>(firstPtr->getType())->typeParam(0);
  Value * firstVal = cg.genExpr(firstPtr);
  firstVal = cg.builder().CreatePtrToInt(firstVal, llvm::Type::Int32Ty);
  Value * lastVal = cg.genExpr(lastPtr);
  lastVal = cg.builder().CreatePtrToInt(lastVal, llvm::Type::Int32Ty);
  Value * diffVal = cg.builder().CreateSub(lastVal, firstVal);
  llvm::Constant * elemSize = cg.genSizeOf(elemType);
  return cg.builder().CreateSDiv(diffVal, elemSize, "ptrDiff");
}

// -------------------------------------------------------------------
// PointerComparisonIntrinsic
template<llvm::CmpInst::Predicate pred>
Expr * PointerComparisonIntrinsic<pred>::eval(const SourceLocation & loc, Expr * self,
    const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 2);
  return new CompareExpr(loc, pred, args[0], args[1]);
}

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_EQ>
    PointerComparisonIntrinsic<CmpInst::ICMP_EQ>::instance("infixEQ");

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_NE>
    PointerComparisonIntrinsic<CmpInst::ICMP_NE>::instance("infixNE");

// -------------------------------------------------------------------
// MathIntrinsic1
template<llvm::Intrinsic::ID id>
inline Value * MathIntrinsic1<id>::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const PrimitiveType * argType = cast<PrimitiveType>(dealias(arg->getType()));
  Value * argVal = cg.genExpr(arg);
  const Type * retType = dealias(call->getType());

  if (argType->getTypeId() != TypeId_Float && argType->getTypeId() != TypeId_Double) {
    diag.fatal(arg->getLocation()) << "Bad intrinsic type.";
    return NULL;
  }

  const llvm::Type * types[1];
  types[1] = argType->getIRType();
  Function * intrinsic = llvm::Intrinsic::getDeclaration(cg.irModule(), id, types, 1);
  return cg.builder().CreateCall(intrinsic, argVal);
}

template<>
MathIntrinsic1<llvm::Intrinsic::sin>
    MathIntrinsic1<llvm::Intrinsic::sin>::instance("tart.core.Math.sin");

template<>
MathIntrinsic1<llvm::Intrinsic::cos>
    MathIntrinsic1<llvm::Intrinsic::cos>::instance("tart.core.Math.cos");

template<>
MathIntrinsic1<llvm::Intrinsic::sqrt>
    MathIntrinsic1<llvm::Intrinsic::sqrt>::instance("tart.core.Math.sqrt");

} // namespace tart
