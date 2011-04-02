/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"

#include "tart/Expr/Expr.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Expr/Constant.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"
#include "tart/Type/TupleType.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/Objects/Intrinsics.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/AnalyzerBase.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Support/raw_ostream.h"

namespace tart {

using namespace llvm;

namespace {

// -------------------------------------------------------------------
// Often intrinsics need to dereference input params
const Expr * derefMacroParam(const Expr * in) {
  if (const LValueExpr * lval = dyn_cast<LValueExpr>(in)) {
    if (lval->value()->defnType() == Defn::Let || lval->value()->defnType() == Defn::MacroArg) {
      const VariableDefn * defn = static_cast<const VariableDefn *> (lval->value());
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

Intrinsic * Intrinsic::get(const SourceLocation & loc, const char * name) {
  static bool init = false;
  if (!init) {
    init = true;
  }

  llvm::StringMap<Intrinsic *>::const_iterator it = intrinsicMap.find(name);
  if (it != intrinsicMap.end()) {
    return it->second;
  }

  diag.fatal(loc) << "Unknown intrinsic function '" << name << "'";
  return NULL;
}

// -------------------------------------------------------------------
// TypecastIntrinsic
TypecastIntrinsic TypecastIntrinsic::instance;

Expr * TypecastIntrinsic::eval(const SourceLocation & loc, Module * m, const FunctionDefn * method,
    Expr * self, const ExprList & args, Type * expectedReturn) const {
  DASSERT(args.size() == 1);
  DASSERT(method->isTemplateInstance());
  DASSERT(method->templateInstance()->typeArgs()->size() == 1);
  Expr * fromExpr = args[0];
  const Type * fromType = dealias(fromExpr->type());
  const Type * toType = dealias(method->templateInstance()->typeArg(0));
  if (const UnionType * utFrom = dyn_cast<UnionType>(fromType)) {
    Expr * castExpr = utFrom->createDynamicCast(fromExpr, toType);
    if (castExpr != NULL) {
      return castExpr;
    }
  }

  return toType->explicitCast(loc, fromExpr, Conversion::Coerce | Conversion::Checked);
}

// -------------------------------------------------------------------
// TypeOfIntrinsic
TypeOfIntrinsic TypeOfIntrinsic::instance;

Value * TypeOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const TypeLiteralExpr * type = cast<TypeLiteralExpr>(arg);
  return cg.getTypeObjectPtr(type->value());
}

// -------------------------------------------------------------------
// CompositeTypeOfIntrinsic
CompositeTypeOfIntrinsic CompositeTypeOfIntrinsic::instance;

Value * CompositeTypeOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const TypeLiteralExpr * typeLiteral = cast<TypeLiteralExpr>(arg);
  return cg.getCompositeTypeObjectPtr(cast<CompositeType>(typeLiteral->value()));
}

// -------------------------------------------------------------------
// TraceTableOfIntrinsic
TraceTableOfIntrinsic TraceTableOfIntrinsic::instance;

Value * TraceTableOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const TypeLiteralExpr * typeLiteral = cast<TypeLiteralExpr>(arg);
  const Type * type = typeLiteral->value();
  Value * indices[2];
  indices[0] = indices[1] = cg.getInt32Val(0);
  llvm::Constant * traceTable = cg.getTraceTable(type);
  if (traceTable != NULL) {
    return llvm::ConstantExpr::getInBoundsGetElementPtr(traceTable, indices, 2);
  } else {
    return NULL;
  }
}

// -------------------------------------------------------------------
// StringifyIntrinsic
StringifyIntrinsic StringifyIntrinsic::instance;

Value * StringifyIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  std::stringstream sstream;
  FormatStream fs(sstream);

  const Expr * arg = derefMacroParam(call->arg(0));
  fs << arg;

  fs.flush();
  DASSERT_OBJ(!sstream.str().empty(), arg);
  return cg.genStringLiteral(sstream.str());
}

// -------------------------------------------------------------------
// PrimitiveToStringIntrinsic
PrimitiveToStringIntrinsic PrimitiveToStringIntrinsic::instance;

Value * PrimitiveToStringIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const FunctionDefn * fn = call->function();
  const Expr * self = call->selfArg();
  //const Expr * formatString = call->arg(0);

  Value * selfArg = cg.genExpr(self);
  //Value * formatStringArg = cg.genExpr(formatString);

  const PrimitiveType * ptype = cast<PrimitiveType>(dealias(self->type()));
  TypeId id = ptype->typeId();

  if (functions_[id] == NULL) {
    char funcName[32];
    DASSERT(ptype != &UnsizedIntType::instance);
    snprintf(funcName, sizeof funcName, "%s_toString", ptype->typeDefn()->name());

    const llvm::FunctionType * funcType = cast<llvm::FunctionType>(fn->type()->irType());
    functions_[id] = cast<llvm::Function>(cg.irModule()->getOrInsertFunction(funcName, funcType));
//    functions_[id] = llvm::Function::Create(funcType),
//        Function::ExternalLinkage, funcName, cg.irModule());
  }

  return cg.builder().CreateCall(functions_[id], selfArg /*, formatStringArg*/);
}

// -------------------------------------------------------------------
// PrimitiveParseIntrinsic
PrimitiveParseIntrinsic PrimitiveParseIntrinsic::instance;

Value * PrimitiveParseIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const FunctionDefn * fn = call->function();
  const Expr * self = call->selfArg();

  //Value * selfArg = cg.genExpr(self);
  Value * strArg = cg.genExpr(call->arg(0));

  if (strArg == NULL) {
    return NULL;
  }

  const PrimitiveType * ptype = cast<PrimitiveType>(fn->returnType());
  TypeId id = ptype->typeId();

  if (functions_[id] == NULL) {
    DASSERT(ptype != &UnsizedIntType::instance);
    char funcName[48];
    snprintf(funcName, sizeof funcName, "tart.core.Strings.parse_%s", ptype->typeDefn()->name());
    const llvm::FunctionType * funcType = cast<llvm::FunctionType>(fn->type()->irType());
    functions_[id] = cast<llvm::Function>(cg.irModule()->getOrInsertFunction(funcName, funcType));
  }

  if (call->argCount() == 2) {
    Value * radixArg = cg.genExpr(call->arg(1));
    return cg.builder().CreateCall2(functions_[id], strArg, radixArg, "parse");
  } else {
    return cg.builder().CreateCall(functions_[id], strArg, "parse");
  }
}

// -------------------------------------------------------------------
// LocationOfIntrinsic
LocationOfIntrinsic LocationOfIntrinsic::instance;

Value * LocationOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  llvm::SmallString<128> str;
  llvm::raw_svector_ostream strm(str);

  const Expr * arg = derefMacroParam(call->arg(0));
  SourceLocation loc = arg->location();
  if (loc.file != NULL) {
    TokenPosition pos = loc.file->tokenPosition(loc);
    strm << loc.file->getFilePath() << ":" << pos.beginLine << ":";
  }

  return cg.genStringLiteral(strm.str());
}

// -------------------------------------------------------------------
// ThisModuleIntrinsic
ThisModuleIntrinsic ThisModuleIntrinsic::instance;

Value * ThisModuleIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  return cg.createModuleObjectPtr();
}

// -------------------------------------------------------------------
// ModuleOfIntrinsic
ModuleOfIntrinsic ModuleOfIntrinsic::instance;

Value * ModuleOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const TypeLiteralExpr * type = cast<TypeLiteralExpr>(arg);
  return cg.createModuleObjectPtr(type->value()->typeDefn()->module());
}

// -------------------------------------------------------------------
// ThisPackageIntrinsic
ThisPackageIntrinsic ThisPackageIntrinsic::instance;

Expr * ThisPackageIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  callingModule->addSymbol(
      AnalyzerBase::getArrayTypeForElement(Builtins::typeModule.get())->typeDefn());
  callingModule->addSymbol(
      AnalyzerBase::getArrayTypeForElement(Builtins::typePackage.get())->typeDefn());
  Expr * emptyModuleArray =
      AnalyzerBase::getEmptyArrayOfElementType(Builtins::typeModule.get());
  Expr * emptyPackageArray =
      AnalyzerBase::getEmptyArrayOfElementType(Builtins::typePackage.get());

  DASSERT(emptyModuleArray != NULL);
  DASSERT(emptyPackageArray != NULL);

  //AnalyzerBase::analyzeDefn(emptyModuleArray, Task_PrepCodeGeneration);
  //AnalyzerBase::analyzeDefn(emptyPackageArray, Task_PrepCodeGeneration);

  return NULL;
}

Value * ThisPackageIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  return cg.createPackageObjectPtr();
}

// -------------------------------------------------------------------
// PackageOfIntrinsic
PackageOfIntrinsic PackageOfIntrinsic::instance;

Expr * PackageOfIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  // TODO: Refactor, combine with above.
  callingModule->addSymbol(
      AnalyzerBase::getArrayTypeForElement(Builtins::typeModule.get())->typeDefn());
  callingModule->addSymbol(
      AnalyzerBase::getArrayTypeForElement(Builtins::typePackage.get())->typeDefn());
  Expr * emptyModuleArray =
      AnalyzerBase::getEmptyArrayOfElementType(Builtins::typeModule.get());
  Expr * emptyPackageArray =
      AnalyzerBase::getEmptyArrayOfElementType(Builtins::typePackage.get());

  DASSERT(emptyModuleArray != NULL);
  DASSERT(emptyPackageArray != NULL);

  //AnalyzerBase::analyzeDefn(emptyModuleArray, Task_PrepCodeGeneration);
  //AnalyzerBase::analyzeDefn(emptyPackageArray, Task_PrepCodeGeneration);

  return NULL;
}

Value * PackageOfIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const TypeLiteralExpr * type = cast<TypeLiteralExpr>(arg);
  return cg.createModuleObjectPtr();
  return cg.createPackageObjectPtr(type->value()->typeDefn()->module());
}

// -------------------------------------------------------------------
// AllocIntrinsic
DefaultAllocIntrinsic DefaultAllocIntrinsic::instance;

Value * DefaultAllocIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  return cg.defaultAlloc(call->arg(0));
}

// -------------------------------------------------------------------
// FlexAllocIntrinsic
FlexAllocIntrinsic FlexAllocIntrinsic::instance;

Value * FlexAllocIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Type * objType = dealias(call->type());
  if (objType->typeClass() != Type::Class) {
    diag.fatal(call->location()) << "__flexAalloc can only be used with classes.";
    return NULL;
  }

  const CompositeType * ctype = cast<CompositeType>(objType);
  Value * count = cg.genExpr(call->arg(0));
  if (count == NULL) {
    return NULL;
  }

  if (ctype->instanceFields().empty()) {
    diag.fatal(call->location()) << "Type " << ctype << " has no FlexibleArray member";
    return NULL;
  }

  size_t lastMemberIndex = ctype->instanceFields().size() - 1;
  VariableDefn * lastField = cast<VariableDefn>(ctype->instanceFields().back());
  if (lastField->type()->typeClass() != Type::FlexibleArray) {
    diag.fatal(call->location()) << "Last member of type " << ctype <<
        " must be of type FlexibleArray";
    return NULL;
  }

  Constant * zero = cg.getInt32Val(0);
  Value * zeroPtr = llvm::ConstantExpr::getIntToPtr(zero, ctype->irEmbeddedType());

  Value * indices[3];
  indices[0] = zero;
  indices[1] = cg.getInt32Val(lastMemberIndex);
  indices[2] = count;
  Value * size = cg.builder().CreateGEP(zeroPtr, &indices[0], &indices[3], "flexSize");

  return cg.genVarSizeAlloc(objType, size);
}

// -------------------------------------------------------------------
// NullObjectIntrinsic
NullObjectIntrinsic NullObjectIntrinsic::instance;

Value * NullObjectIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Type * retType = dealias(call->type());
  const llvm::Type * type = retType->irType();
  return ConstantPointerNull::get(type->getPointerTo());
}

// -------------------------------------------------------------------
// PtrToIntIntrinsic
PtrToIntIntrinsic PtrToIntIntrinsic::instance;

Value * PtrToIntIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 1);
  Value * val = cg.genExpr(call->arg(0));
  return cg.builder().CreatePtrToInt(val, call->type()->irType(), "ptrToInt");
}

// -------------------------------------------------------------------
// ReinterpretPtrIntrinsic
ReinterpretPtrIntrinsic ReinterpretPtrIntrinsic::instance;

Value * ReinterpretPtrIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 1);
  Value * val = cg.genExpr(call->arg(0));
  return cg.builder().CreatePointerCast(val, call->type()->irType(), "ptrToPtr");
}

// -------------------------------------------------------------------
// BitCastIntrinsic
BitCastIntrinsic BitCastIntrinsic::instance;

Value * BitCastIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 1);
  Value * val = cg.genExpr(call->arg(0));
  return cg.builder().CreateBitCast(val, call->type()->irEmbeddedType(), "bitCast");
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
// ObjectAddressIntrinsic
ObjectAddressIntrinsic ObjectAddressIntrinsic::instance;

Value * ObjectAddressIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 1);
  Value * val = cg.genExpr(call->arg(0));
  return cg.builder().CreatePointerCast(val, call->type()->irType(), "objectAddress");
}

// -------------------------------------------------------------------
// ObjectReferenceIntrinsic
ObjectReferenceIntrinsic ObjectReferenceIntrinsic::instance;

Value * ObjectReferenceIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 1);
  Value * val = cg.genExpr(call->arg(0));
  return cg.builder().CreatePointerCast(val, call->type()->irEmbeddedType(), "objectReference");
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

  DASSERT_OBJ(firstPtr->type()->isEqual(lastPtr->type()), call);
  const Type * elemType = firstPtr->type()->typeParam(0);
  Value * firstVal = cg.genExpr(firstPtr);
  Value * lastVal = cg.genExpr(lastPtr);
  Value * diffVal = cg.builder().CreatePtrDiff(lastVal, firstVal, "ptrDiff");
  if (call->type() == &Int32Type::instance) {
    return cg.builder().CreateTrunc(diffVal, cg.builder().getInt32Ty());
  } else {
    return diffVal;
  }
}

// -------------------------------------------------------------------
// DerefIntrinsic
DerefIntrinsic DerefIntrinsic::instance;

Expr * DerefIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  DASSERT(args.size() == 1);
  Expr * arg = args[0];
  return new UnaryExpr(Expr::PtrDeref, loc, dealias(arg->type())->typeParam(0), arg);
}

// -------------------------------------------------------------------
// PointerComparisonIntrinsic
template<llvm::CmpInst::Predicate pred>
Expr * PointerComparisonIntrinsic<pred>::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 2);
  return new CompareExpr(loc, pred, args[0], args[1]);
}

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_EQ>
PointerComparisonIntrinsic<CmpInst::ICMP_EQ>::instance("infixEQ");

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_NE>
PointerComparisonIntrinsic<CmpInst::ICMP_NE>::instance("infixNE");

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_ULE>
PointerComparisonIntrinsic<CmpInst::ICMP_ULE>::instance("infixLE");

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_ULT>
PointerComparisonIntrinsic<CmpInst::ICMP_ULT>::instance("infixLT");

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_UGT>
PointerComparisonIntrinsic<CmpInst::ICMP_UGT>::instance("infixGT");

template<>
PointerComparisonIntrinsic<CmpInst::ICMP_UGE>
PointerComparisonIntrinsic<CmpInst::ICMP_UGE>::instance("infixGE");

// -------------------------------------------------------------------
// AddressAddIntrinsic
AddressAddIntrinsic AddressAddIntrinsic::instance;

Value * AddressAddIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 2);
  Value * ptr = cg.genExpr(call->arg(0));
  Value * offset = cg.genExpr(call->arg(1));
  return cg.builder().CreateGEP(ptr, &offset, (&offset) + 1, "ptr_add");
}

// -------------------------------------------------------------------
// LogicalAndIntrinsic
LogicalAndIntrinsic LogicalAndIntrinsic::instance;

Expr * LogicalAndIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 2);
  Expr * first = args[0];
  Expr * second = args[1];

  enum {
    Unknown,
    True,
    False,
  };

  int constFirst = Unknown;
  int constSecond = Unknown;

  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(first)) {
    constFirst = cint->value() != 0 ? True : False;
  }

  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(second)) {
    constSecond = cint->value() != 0 ? True : False;
  }

  if (constSecond == False || constSecond == False) {
    return ConstantInteger::getConstantBool(loc, false);
  }

  return new BinaryExpr(Expr::And, loc, &BoolType::instance, first, second);
}

// -------------------------------------------------------------------
// LogicalOrIntrinsic
LogicalOrIntrinsic LogicalOrIntrinsic::instance;

Expr * LogicalOrIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 2);
  Expr * first = args[0];
  Expr * second = args[1];

  enum {
    Unknown,
    True,
    False,
  };

  int constFirst = Unknown;
  int constSecond = Unknown;

  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(first)) {
    constFirst = cint->value() != 0 ? True : False;
  }

  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(second)) {
    constSecond = cint->value() != 0 ? True : False;
  }

  if (constSecond == True || constSecond == True) {
    return ConstantInteger::getConstantBool(loc, true);
  }

  return new BinaryExpr(Expr::Or, loc, &BoolType::instance, first, second);
}

// -------------------------------------------------------------------
// ArrayCopyIntrinsic
ArrayCopyIntrinsic ArrayCopyIntrinsic::copyInstance(
    "tart.core.Memory.arrayCopy", llvm::Intrinsic::memcpy);
ArrayCopyIntrinsic ArrayCopyIntrinsic::moveInstance(
    "tart.core.Memory.arrayMove", llvm::Intrinsic::memmove);

Value * ArrayCopyIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 3);
  const Expr * dstArray = call->arg(0);
  const Expr * srcArray = call->arg(1);
  const Expr * count = call->arg(2);

  DASSERT_OBJ(srcArray->type()->isEqual(dstArray->type()), call);
  const Type * elemType = srcArray->type()->typeParam(0);
  Value * srcPtr = cg.genExpr(srcArray);
  Value * dstPtr = cg.genExpr(dstArray);
  Value * length = cg.genExpr(count);

  Value * elemSize = cg.builder().CreateTruncOrBitCast(
      llvm::ConstantExpr::getSizeOf(elemType->irEmbeddedType()),
      length->getType());

  const llvm::Type * int8PtrType = cg.builder().getInt8Ty()->getPointerTo();

  const llvm::Type * types[3];
  types[0] = int8PtrType;
  types[1] = int8PtrType;
  types[2] = length->getType();
  Function * intrinsic = llvm::Intrinsic::getDeclaration(cg.irModule(), _id, types, 3);

  Value * args[5];
  args[0] = cg.builder().CreatePointerCast(dstPtr, int8PtrType);
  args[1] = cg.builder().CreatePointerCast(srcPtr, int8PtrType);
  args[2] = cg.builder().CreateMul(length, elemSize);
  args[3] = cg.getInt32Val(0); // TODO: Better alignment
  args[4] = llvm::ConstantInt::getFalse(cg.context()); // TODO: isVolatile

  return cg.builder().CreateCall(intrinsic, &args[0], &args[5]);
}

// -------------------------------------------------------------------
// MathIntrinsic1i
template<llvm::Intrinsic::ID id>
Expr * MathIntrinsic1i<id>::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  if (const ConstantInteger * arg = dyn_cast<ConstantInteger>(args[0])) {
    const PrimitiveType * type = arg->primitiveType();
    llvm::APInt value = arg->value()->getValue();
    DASSERT(value.getBitWidth() == type->numBits());
    uint32_t result = 0;

    switch (id) {
      case llvm::Intrinsic::ctlz:
        result = value.countLeadingZeros();
        break;

      case llvm::Intrinsic::cttz:
        result = value.countTrailingZeros();
        break;

      default:
        return NULL;
    }

    return ConstantInteger::get(loc, type, result);
  }

  return NULL;
}

template<llvm::Intrinsic::ID id>
inline Value * MathIntrinsic1i<id>::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const PrimitiveType * argType = cast<PrimitiveType>(dealias(arg->type()));
  Value * argVal = cg.genExpr(arg);
  const Type * retType = dealias(call->type());

  if (argType->typeId() != TypeId_SInt32 && argType->typeId() != TypeId_SInt64) {
    diag.fatal(arg->location()) << "Bad intrinsic type.";
    return NULL;
  }

  const llvm::Type * types[1];
  types[0] = argType->irType();
  Function * intrinsic = llvm::Intrinsic::getDeclaration(cg.irModule(), id, types, 1);
  return cg.builder().CreateCall(intrinsic, argVal);
}

template<>
MathIntrinsic1i<llvm::Intrinsic::ctlz>
MathIntrinsic1i<llvm::Intrinsic::ctlz>::instance("tart.core.BitTricks.leadingZeroes");

template<>
MathIntrinsic1i<llvm::Intrinsic::cttz>
MathIntrinsic1i<llvm::Intrinsic::cttz>::instance("tart.core.BitTricks.trailingZeroes");

// -------------------------------------------------------------------
// MathIntrinsic1f
template<llvm::Intrinsic::ID id>
inline Value * MathIntrinsic1f<id>::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg = call->arg(0);
  const PrimitiveType * argType = cast<PrimitiveType>(dealias(arg->type()));
  Value * argVal = cg.genExpr(arg);
  const Type * retType = dealias(call->type());

  if (argType->typeId() != TypeId_Float && argType->typeId() != TypeId_Double) {
    diag.fatal(arg->location()) << "Bad intrinsic type.";
    return NULL;
  }

  const llvm::Type * types[1];
  types[0] = argType->irType();
  Function * intrinsic = llvm::Intrinsic::getDeclaration(cg.irModule(), id, types, 1);
  return cg.builder().CreateCall(intrinsic, argVal);
}

template<>
MathIntrinsic1f<llvm::Intrinsic::sin>
MathIntrinsic1f<llvm::Intrinsic::sin>::instance("tart.core.Math.sin");

template<>
MathIntrinsic1f<llvm::Intrinsic::cos>
MathIntrinsic1f<llvm::Intrinsic::cos>::instance("tart.core.Math.cos");

template<>
MathIntrinsic1f<llvm::Intrinsic::sqrt>
MathIntrinsic1f<llvm::Intrinsic::sqrt>::instance("tart.core.Math.sqrt");

// -------------------------------------------------------------------
// MathIntrinsic2f
template<llvm::Intrinsic::ID id>
inline Value * MathIntrinsic2f<id>::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  const Expr * arg0 = call->arg(0);
  const Expr * arg1 = call->arg(1);
  const PrimitiveType * arg0Type = cast<PrimitiveType>(dealias(arg0->type()));
  const PrimitiveType * arg1Type = cast<PrimitiveType>(dealias(arg1->type()));
  const Type * retType = dealias(call->type());

  if (arg0Type->typeId() != TypeId_Float && arg0Type->typeId() != TypeId_Double) {
    diag.fatal(arg0->location()) << "Bad intrinsic type.";
    return NULL;
  }

  if (arg1Type->typeId() != TypeId_Float && arg1Type->typeId() != TypeId_Double) {
    diag.fatal(arg1->location()) << "Bad intrinsic type.";
    return NULL;
  }

  Value * arg0Val = cg.genExpr(arg0);
  Value * arg1Val = cg.genExpr(arg1);

  const llvm::Type * types[2];
  types[0] = arg0Type->irType();
  types[1] = arg1Type->irType();
  Function * intrinsic = llvm::Intrinsic::getDeclaration(cg.irModule(), id, types, 2);
  return cg.builder().CreateCall(intrinsic, arg0Val, arg1Val);
}

template<>
MathIntrinsic1f<llvm::Intrinsic::pow>
MathIntrinsic1f<llvm::Intrinsic::pow>::instance("tart.core.Math.pow");

// -------------------------------------------------------------------
// AtomicCasIntrinsic

AtomicCasIntrinsic AtomicCasIntrinsic::instance_int("tart.atomic.AtomicInt.cas");
AtomicCasIntrinsic AtomicCasIntrinsic::instance_ptr("tart.atomic.AtomicPtr.cas");

llvm::Value * AtomicCasIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 2);
  const Expr * self = call->selfArg();
  const Expr * cmp = call->arg(0);
  const Expr * val = call->arg(1);

  Value * selfValue = cg.genLValueAddress(self);
  Value * theValue = cg.builder().CreateConstInBoundsGEP2_32(selfValue, 0, 0, "value");
  Value * cmpValue = cg.genExpr(cmp);
  Value * valValue = cg.genExpr(val);

  const llvm::Type * types[2];
  types[0] = cmpValue->getType();
  types[1] = theValue->getType();
  Function * intrinsic = llvm::Intrinsic::getDeclaration(
      cg.irModule(), llvm::Intrinsic::atomic_cmp_swap, types, 2);
  Value * resultVal = cg.builder().CreateCall3(intrinsic, theValue, cmpValue, valValue, "");
  return cg.builder().CreateICmpEQ(resultVal, cmpValue);
}

// -------------------------------------------------------------------
// FlagsApplyIntrinsic
FlagsApplyIntrinsic FlagsApplyIntrinsic::instance;

Expr * FlagsApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  TypeLiteralExpr * ctype = cast<TypeLiteralExpr>(args[0]);
  EnumType * enumType = cast<EnumType>(const_cast<Type *>(ctype->value()));
  enumType->setIsFlags(true);
  return args[0];
}

// -------------------------------------------------------------------
// ExternApplyIntrinsic
ExternApplyIntrinsic ExternApplyIntrinsic::instance;

Expr * ExternApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  ConstantObjectRef * selfObj = cast<ConstantObjectRef>(self);
  const ConstantString * extName = dyn_cast<ConstantString>(selfObj->members()[2]);
  DASSERT_OBJ(extName != NULL, self);

  LValueExpr * lval = cast<LValueExpr>(args[0]);
  if (FunctionDefn * fn = dyn_cast<FunctionDefn>(lval->value())) {
    fn->setFlag(FunctionDefn::Extern);
    fn->setLinkageName(extName->value());
  } else if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
    var->setFlag(VariableDefn::Extern);
    var->setLinkageName(extName->value());
  } else {
    diag.error(loc) << "Invalid target for 'Extern' attribute.";
  }
  return self;
}

// -------------------------------------------------------------------
// LinkageNameApplyIntrinsic
LinkageNameApplyIntrinsic LinkageNameApplyIntrinsic::instance;

Expr * LinkageNameApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  ConstantObjectRef * selfObj = cast<ConstantObjectRef>(self);
  const ConstantString * linkName = cast<ConstantString>(selfObj->members()[2]);
  LValueExpr * lval = cast<LValueExpr>(args[0]);
  lval->value()->setLinkageName(linkName->value());
  return self;
}

// -------------------------------------------------------------------
// EntryPointApplyIntrinsic
EntryPointApplyIntrinsic EntryPointApplyIntrinsic::instance;

Expr * EntryPointApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  ConstantObjectRef * selfObj = cast<ConstantObjectRef>(self);
  LValueExpr * lval = cast<LValueExpr>(args[0]);
  FunctionDefn * fn = cast<FunctionDefn>(lval->value());

  TypeDefn * entryPointType = cast<TypeDefn>(method->parentDefn());
  FunctionDefn * programStartFn = cast<FunctionDefn>(
      entryPointType->typeValue()->memberScope()->lookupSingleMember("programStart", false));
  DASSERT(programStartFn != NULL);

  Module * module = fn->module();
  if (module->entryPoint() != NULL) {
    diag.error(fn) << "@EntryPoint attribute conflicts with earlier entry point: " <<
    module->entryPoint();
  } else {
    module->setEntryPoint(fn);
    module->setProgramStartup(programStartFn);
  }

  return self;
}

// -------------------------------------------------------------------
// EssentialApplyIntrinsic
EssentialApplyIntrinsic EssentialApplyIntrinsic::instance;

Expr * EssentialApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  TypeLiteralExpr * ctype = cast<TypeLiteralExpr>(args[0]);
  Builtins::registerEssentialType(ctype->value());
  return ctype;
}

// -------------------------------------------------------------------
// GenerateStackTraceApplyIntrinsic
GenerateStackTraceApplyIntrinsic GenerateStackTraceApplyIntrinsic::instance;

Expr * GenerateStackTraceApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  LValueExpr * lval = cast<LValueExpr>(args[0]);
  if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
    if (var->defnType() != Defn::Let) {
      diag.error(loc) << "Invalid target for attribute: " << lval;
    } else {
      var->addTrait(Defn::RequestStackTrace);
    }
  } else {
    diag.error(loc) << "Invalid target for attribute: " << lval;
  }

  return args[0];
}

// -------------------------------------------------------------------
// UnsafeApplyIntrinsic
UnsafeApplyIntrinsic UnsafeApplyIntrinsic::instance;

Expr * UnsafeApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (TypeLiteralExpr * ctype = dyn_cast<TypeLiteralExpr>(args[0])) {
    if (TypeDefn * tdef = ctype->value()->typeDefn()) {
      tdef->addTrait(Defn::Unsafe);
    }
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    lval->value()->addTrait(Defn::Unsafe);
  } else {
    diag.error(loc) << "Invalid target for 'Unsafe'";
  }

  return args[0];
}

// -------------------------------------------------------------------
// CoalesceApplyIntrinsic
CoalesceApplyIntrinsic CoalesceApplyIntrinsic::instance;

Expr * CoalesceApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (TypeLiteralExpr * ctype = dyn_cast<TypeLiteralExpr>(args[0])) {
    if (TypeDefn * tdef = ctype->value()->typeDefn()) {
      tdef->addTrait(Defn::Mergeable);
    }
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    lval->value()->addTrait(Defn::Mergeable);
  } else {
    diag.fatal(loc) << "Invalid target for @Coalesce.";
    return args[0];
  }

  return args[0];
}

// -------------------------------------------------------------------
// ReflectApplyIntrinsic
ReflectApplyIntrinsic ReflectApplyIntrinsic::instance;

Expr * ReflectApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (TypeLiteralExpr * ctype = dyn_cast<TypeLiteralExpr>(args[0])) {
    if (TypeDefn * tdef = ctype->value()->typeDefn()) {
      tdef->addTrait(Defn::Reflect);
    }
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    lval->value()->addTrait(Defn::Reflect);
  } else {
    diag.fatal(loc) << "Invalid target for @Reflect.";
  }

  return args[0];
}

// -------------------------------------------------------------------
// TargetPropertyApplyIntrinsic
TargetPropertyApplyIntrinsic TargetPropertyApplyIntrinsic::instance;

Expr * TargetPropertyApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {

    } else {
      diag.fatal(loc) << "Invalid target for @TargetProperty.";
    }
    //lval->value()->addTrait(Defn::TargetProperty);
  } else {
    diag.fatal(loc) << "Invalid target for @TargetProperty.";
  }

  return args[0];
}

// -------------------------------------------------------------------
// ThreadLocalIntrinsic
ThreadLocalApplyIntrinsic ThreadLocalApplyIntrinsic::instance;

Expr * ThreadLocalApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
      if (var->storageClass() == Storage_Static || var->storageClass() == Storage_Global) {
        var->setThreadLocal(true);
        return args[0];
      }
    }
  }

  diag.error(loc) << "Invalid target for 'ThreadLocal'";
  return args[0];
}

// -------------------------------------------------------------------
// TraceMethodApplyIntrinsic
TraceMethodApplyIntrinsic TraceMethodApplyIntrinsic::instance;

Expr * TraceMethodApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    if (FunctionDefn * fn = dyn_cast<FunctionDefn>(lval->value())) {
      if (fn->storageClass() == Storage_Instance) {
        CompositeType * ctype = fn->definingClass();
        DASSERT_OBJ(ctype != NULL, fn);
        ctype->traceMethods().push_back(fn);
        //callingModule->addSymbol(fn);
        return args[0];
      }
    }
  }

  diag.error(loc) << "Invalid target for 'TraceMethod'";
  return args[0];
}

// -------------------------------------------------------------------
// NoInlineApplyIntrinsic
NoInlineApplyIntrinsic NoInlineApplyIntrinsic::instance;

Expr * NoInlineApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    if (FunctionDefn * fn = dyn_cast<FunctionDefn>(lval->value())) {
      fn->setFlag(FunctionDefn::NoInline, true);
      return args[0];
    }
  }

  diag.error(loc) << "Invalid target for 'NoInline'";
  return args[0];
}

// -------------------------------------------------------------------
// AssociativeApplyIntrinsic
AssociativeApplyIntrinsic AssociativeApplyIntrinsic::instance;

Expr * AssociativeApplyIntrinsic::eval(const SourceLocation & loc, Module * callingModule,
    const FunctionDefn * method, Expr * self, const ExprList & args, Type * expectedReturn) const {
  assert(args.size() == 1);
  if (LValueExpr * lval = dyn_cast<LValueExpr>(args[0])) {
    if (FunctionDefn * fn = dyn_cast<FunctionDefn>(lval->value())) {
      //fn->setFlag(FunctionDefn::Associative, true);
      return args[0];
    }
  }

  diag.error(loc) << "Invalid target for 'Associative'";
  return args[0];
}

// -------------------------------------------------------------------
// ProxyCreateIntrinsic
ProxyCreateIntrinsic ProxyCreateIntrinsic::instance;

Value * ProxyCreateIntrinsic::generate(CodeGenerator & cg, const FnCallExpr * call) const {
  DASSERT(call->argCount() == 2);
  const Expr * typeArg = call->arg(0);
  const TypeLiteralExpr * typeLiteral = cast<TypeLiteralExpr>(typeArg);
  const Type * type = typeLiteral->value();
  if (!type->isReferenceType()) {
    diag.error(call) << "Only reference types can be proxied.";
    return NULL;
  }

  // Handler arg will be an InvocationHandler[T].
  const Expr * handlerArg = call->arg(1);

  llvm::Constant * proxyTib = cg.genProxyType(static_cast<const CompositeType *>(type));

  // Note: We want to make sure that we generate this only once.

  //return cg.createTypeObjectPtr(type->value());
  DFAIL("Implement");
}

} // namespace tart
