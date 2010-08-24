/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_INTRINSICS_H
#define TART_OBJECTS_INTRINSICS_H

#ifndef TART_OBJECTS_INTRINSIC_H
#include "tart/Objects/Intrinsic.h"
#endif

namespace tart {

// -------------------------------------------------------------------
// typecast intrinsic
class TypecastIntrinsic : public Intrinsic {
  static TypecastIntrinsic instance;
  TypecastIntrinsic() : Intrinsic("tart.core.typecast") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Type.of intrinsic
class TypeOfIntrinsic : public Intrinsic {
  static TypeOfIntrinsic instance;
  TypeOfIntrinsic() : Intrinsic("tart.reflect.Type.of") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// ComplexType.of intrinsic
class ComplexTypeOfIntrinsic : public Intrinsic {
  static ComplexTypeOfIntrinsic instance;
  ComplexTypeOfIntrinsic() : Intrinsic("tart.reflect.ComplexType.of") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Debug.stringify intrinsic
class StringifyIntrinsic : public Intrinsic {
  static StringifyIntrinsic instance;
  StringifyIntrinsic() : Intrinsic("tart.core.Debug.stringify") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  // TODO: Eval() for constants.
};

// -------------------------------------------------------------------
// PrimitiveType.toString() intrinsic
class PrimitiveToStringIntrinsic : public Intrinsic {
  static PrimitiveToStringIntrinsic instance;
  PrimitiveToStringIntrinsic() : Intrinsic("PrimitiveType.toString") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;

  mutable llvm::Function * functions_[TypeId_Count];
  // TODO: Eval() for constants.
};

// -------------------------------------------------------------------
// Debug.locationOf intrinsic
class LocationOfIntrinsic : public Intrinsic {
  static LocationOfIntrinsic instance;
  LocationOfIntrinsic() : Intrinsic("tart.core.Debug.locationOf") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Module.thisModule intrinsic
class ThisModuleIntrinsic : public Intrinsic {
  static ThisModuleIntrinsic instance;
  ThisModuleIntrinsic() : Intrinsic("tart.reflect.Module.thisModule") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Module.of intrinsic
class ModuleOfIntrinsic : public Intrinsic {
  static ModuleOfIntrinsic instance;
  ModuleOfIntrinsic() : Intrinsic("tart.reflect.Module.of") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Package.thisPackage intrinsic
class ThisPackageIntrinsic : public Intrinsic {
  static ThisPackageIntrinsic instance;
  ThisPackageIntrinsic() : Intrinsic("tart.reflect.Package.thisPackage") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Package.of intrinsic
class PackageOfIntrinsic : public Intrinsic {
  static PackageOfIntrinsic instance;
  PackageOfIntrinsic() : Intrinsic("tart.reflect.Package.of") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Object.__valloc intrinsic
class VAllocIntrinsic : public Intrinsic {
  static VAllocIntrinsic instance;
  VAllocIntrinsic() : Intrinsic("tart.core.Object.__valloc") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Object.__pvalloc intrinsic
class PVAllocIntrinsic : public Intrinsic {
  static PVAllocIntrinsic instance;
  PVAllocIntrinsic() : Intrinsic("tart.core.Object.__pvalloc") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.zeroPtr intrinsic
class ZeroPtrIntrinsic : public Intrinsic {
  static ZeroPtrIntrinsic instance;
  ZeroPtrIntrinsic() : Intrinsic("tart.core.Memory.zeroPtr") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.ptrToInt intrinsic
class PtrToIntIntrinsic : public Intrinsic {
  static PtrToIntIntrinsic instance;
  PtrToIntIntrinsic() : Intrinsic("tart.core.Memory.ptrToInt") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.reinterpretPtr intrinsic
class ReinterpretPtrIntrinsic : public Intrinsic {
  static ReinterpretPtrIntrinsic instance;
  ReinterpretPtrIntrinsic() : Intrinsic("tart.core.Memory.reinterpretPtr") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.addressOf intrinsic
class AddressOfIntrinsic : public Intrinsic {
  static AddressOfIntrinsic instance;
  AddressOfIntrinsic() : Intrinsic("tart.core.Memory.addressOf") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.objectAddress intrinsic
class ObjectAddressIntrinsic : public Intrinsic {
  static ObjectAddressIntrinsic instance;
  ObjectAddressIntrinsic() : Intrinsic("tart.core.Memory.objectAddress") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.ptrDiff intrinsic
class PointerDiffIntrinsic : public Intrinsic {
  static PointerDiffIntrinsic instance;
  PointerDiffIntrinsic() : Intrinsic("tart.core.Memory.ptrDiff") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Memory.deref intrinsic
class DerefIntrinsic : public Intrinsic {
  static DerefIntrinsic instance;
  DerefIntrinsic() : Intrinsic("tart.core.Memory.deref") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Operator.equal and Operator.unequal for native pointers.
template<llvm::CmpInst::Predicate pred>
class PointerComparisonIntrinsic : public Intrinsic {
  static PointerComparisonIntrinsic instance;
  PointerComparisonIntrinsic(const char * name) : Intrinsic(name) {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Operator.infixAdd native address.
class AddressAddIntrinsic : public Intrinsic {
  static AddressAddIntrinsic instance;
  AddressAddIntrinsic() : Intrinsic("infixAdd") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Operator.infixLogicalAnd intrinsic
class LogicalAndIntrinsic : public Intrinsic {
  static LogicalAndIntrinsic instance;
  LogicalAndIntrinsic() : Intrinsic("infixLogicalAnd") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Operator.infixLogicalOr intrinsic
class LogicalOrIntrinsic : public Intrinsic {
  static LogicalOrIntrinsic instance;
  LogicalOrIntrinsic() : Intrinsic("infixLogicalOr") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Memory.arrayCopy intrinsic
class ArrayCopyIntrinsic : public Intrinsic {
  static ArrayCopyIntrinsic copyInstance;
  static ArrayCopyIntrinsic moveInstance;
  ArrayCopyIntrinsic(const char * name, llvm::Intrinsic::ID id) : Intrinsic(name), _id(id) {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  llvm::Intrinsic::ID _id;
};

// -------------------------------------------------------------------
// Math.sin intrinsic
class MathSinIntrinsic : public Intrinsic {
  static MathSinIntrinsic instance;
  MathSinIntrinsic() : Intrinsic("tart.core.Math.sin") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  // TODO: Eval() for constants.
};

// -------------------------------------------------------------------
// Math.cos intrinsic
class MathCosIntrinsic : public Intrinsic {
  static MathCosIntrinsic instance;
  MathCosIntrinsic() : Intrinsic("tart.core.Math.cos") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  // TODO: Eval() for constants.
};

// -------------------------------------------------------------------
// Math intrinsic with 1 int argument

template<llvm::Intrinsic::ID id>
class MathIntrinsic1i : public Intrinsic {
  static MathIntrinsic1i instance;
  MathIntrinsic1i(const char * name) : Intrinsic(name) {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Math intrinsic with 1 float argument

template<llvm::Intrinsic::ID id>
class MathIntrinsic1f : public Intrinsic {
  static MathIntrinsic1f instance;
  MathIntrinsic1f(const char * name) : Intrinsic(name) {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  // TODO: Eval() for constants.
};

// -------------------------------------------------------------------
// Math intrinsic with 2 float arguments

template<llvm::Intrinsic::ID id>
class MathIntrinsic2f : public Intrinsic {
  static MathIntrinsic2f instance;
  MathIntrinsic2f(const char * name) : Intrinsic(name) {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  // TODO: Eval() for constants.
};

// -------------------------------------------------------------------
// AtomicCas intrinsic
class AtomicCasIntrinsic : public Intrinsic {
  static AtomicCasIntrinsic instance_int;
  static AtomicCasIntrinsic instance_ptr;
  AtomicCasIntrinsic(const char * n) : Intrinsic(n) {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Flags.apply intrinsic
class FlagsApplyIntrinsic : public Intrinsic {
  static FlagsApplyIntrinsic instance;
  FlagsApplyIntrinsic() : Intrinsic("tart.core.Flags.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Extern.apply intrinsic
class ExternApplyIntrinsic : public Intrinsic {
  static ExternApplyIntrinsic instance;
  ExternApplyIntrinsic() : Intrinsic("tart.core.Extern.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// LinkageName.apply intrinsic
class LinkageNameApplyIntrinsic : public Intrinsic {
  static LinkageNameApplyIntrinsic instance;
  LinkageNameApplyIntrinsic() : Intrinsic("tart.core.LinkageName.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// EntryPoint.apply intrinsic
class EntryPointApplyIntrinsic : public Intrinsic {
  static EntryPointApplyIntrinsic instance;
  EntryPointApplyIntrinsic() : Intrinsic("tart.core.EntryPoint.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Essential.apply intrinsic
class EssentialApplyIntrinsic : public Intrinsic {
  static EssentialApplyIntrinsic instance;
  EssentialApplyIntrinsic() : Intrinsic("tart.annex.Essential.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// GenerateStackTrace.apply intrinsic
class GenerateStackTraceApplyIntrinsic : public Intrinsic {
  static GenerateStackTraceApplyIntrinsic instance;
  GenerateStackTraceApplyIntrinsic() : Intrinsic("tart.annex.GenerateStackTrace.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self, const ExprList & args,
      Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Unsafe.apply intrinsic
class UnsafeApplyIntrinsic : public Intrinsic {
  static UnsafeApplyIntrinsic instance;
  UnsafeApplyIntrinsic() : Intrinsic("tart.core.Unsafe.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Reflection.apply intrinsic
class ReflectionApplyIntrinsic : public Intrinsic {
  static ReflectionApplyIntrinsic instance;
  ReflectionApplyIntrinsic() : Intrinsic("tart.reflect.Reflection.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// TargetProperty.apply intrinsic
class TargetPropertyApplyIntrinsic : public Intrinsic {
  static TargetPropertyApplyIntrinsic instance;
  TargetPropertyApplyIntrinsic() : Intrinsic("tart.annex.TargetProperty.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// ThreadLocal.apply intrinsic
class ThreadLocalApplyIntrinsic : public Intrinsic {
  static ThreadLocalApplyIntrinsic instance;
  ThreadLocalApplyIntrinsic() : Intrinsic("tart.concurrent.ThreadLocal.apply") {}
  Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Proxy.create intrinsic
class ProxyCreateIntrinsic : public Intrinsic {
  static ProxyCreateIntrinsic instance;
  ProxyCreateIntrinsic() : Intrinsic("tart.reflect.Proxy.create") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

}

#endif
