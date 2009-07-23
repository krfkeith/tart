/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_OBJECTS_INTRINSIC_H
#define TART_OBJECTS_INTRINSIC_H

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

#include "llvm/Intrinsics.h"

namespace llvm {
  class Value;
}

namespace tart {
  
class CodeGenerator;
class FnCallExpr;

/// -------------------------------------------------------------------
/// Intrinsic functions are ones that are implemented directly in the
/// compiler rather than being specified in tart source. Note that
/// in some cases, several overloaded methods can point to the same
/// instrinsic structure.
///
/// Intrinsics work by replacing the function call with generated code.
/// This replacement can happen either in the analysis phase or in the
/// code generation phase. In the former case, the replacement is done
/// by substituting expression nodes. In the latter case, the replacement
/// is done via a custom code generator function.
class Intrinsic {
private:
  const char * name;
  
  static llvm::StringMap<Intrinsic *> intrinsicMap;

public:
  Intrinsic(const char * n) : name(n) {
    intrinsicMap[name] = this;
  }
  
  virtual ~Intrinsic() {}
  
  /** The fully qualified name of this intrinsic. */
  const char * getName() { return name; }

  /** Analysis-time implementation of the intrinsic. This returns an
      expression node which replaces the function call. Returning NULL
      indicates that no replacement should be done. */
  virtual Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args,
      Type * expectedReturn) const {
    return NULL;
  }

  /** Code-generation implementation of the intrinsic. */
  virtual llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
  
  /** Lookup an intrinsic by name. */
  static Intrinsic * get(const char * name);
};

// -------------------------------------------------------------------
// Debug.stringify intrinsic
class StringifyIntrinsic : public Intrinsic {
  static StringifyIntrinsic instance;
  StringifyIntrinsic() : Intrinsic("tart.core.Debug.stringify") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// PrimitiveType.toString() intrinsic
class PrimitiveToStringIntrinsic : public Intrinsic {
  static PrimitiveToStringIntrinsic instance;
  PrimitiveToStringIntrinsic() : Intrinsic("PrimitiveType.toString") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;

  mutable llvm::Function * functions_[TypeId_Count];
};

// -------------------------------------------------------------------
// Debug.locationOf intrinsic
class LocationOfIntrinsic : public Intrinsic {
  static LocationOfIntrinsic instance;
  LocationOfIntrinsic() : Intrinsic("tart.core.Debug.locationOf") {}
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
// Memory.addressOf intrinsic
class AddressOfIntrinsic : public Intrinsic {
  static AddressOfIntrinsic instance;
  AddressOfIntrinsic() : Intrinsic("tart.core.Memory.addressOf") {}
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
// Operator.equual and Operator.unequal for native pointers.
template<llvm::CmpInst::Predicate pred>
class PointerComparisonIntrinsic : public Intrinsic {
  static PointerComparisonIntrinsic instance;
  PointerComparisonIntrinsic(const char * name) : Intrinsic(name) {}
  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args,
      Type * expectedReturn) const;
};

// -------------------------------------------------------------------
// Math.sin intrinsic
class MathSinIntrinsic : public Intrinsic {
  static MathSinIntrinsic instance;
  MathSinIntrinsic() : Intrinsic("tart.core.Math.sin") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Math.cos intrinsic
class MathCosIntrinsic : public Intrinsic {
  static MathCosIntrinsic instance;
  MathCosIntrinsic() : Intrinsic("tart.core.Math.cos") {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

// -------------------------------------------------------------------
// Math intrinsic with 1 argument
//class MathSqrtIntrinsic : public Intrinsic {
//  static MathSqrtIntrinsic instance;
//  MathSqrtIntrinsic() : Intrinsic("tart.core.Math.sqrt") {}
//  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
//};

template<llvm::Intrinsic::ID id>
class MathIntrinsic1 : public Intrinsic {
  static MathIntrinsic1 instance;
  MathIntrinsic1(const char * name) : Intrinsic(name) {}
  llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;
};

}

#endif
