/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_FUNCTIONTYPE_H
#define TART_CFG_FUNCTIONTYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

namespace tart {

class FunctionDefn;

// -------------------------------------------------------------------
// Type class for functions and macros.
class FunctionType : public Type {
public:
  FunctionType(Type * rtype, ParameterList & plist);
  FunctionType(Type * rtype, ParameterDefn ** plist, size_t pcount);
  FunctionType(Type * rtype, ParameterDefn * selfParam, ParameterDefn ** plist, size_t pcount);

  // isStatic
  bool isStatic() const { return isStatic_; }
  void setIsStatic(bool value) { isStatic_ = value; }

  // Return type
  const Type * returnType() const { return returnType_; }
  void setReturnType(const Type * type) { returnType_ = type; }

  const llvm::Type * irType() const;

  // Parameter methods

  /** The 'self' parameter of the function. */
  ParameterDefn * selfParam() const { return selfParam_; }
  void setSelfParam(ParameterDefn * self) { selfParam_ = self; }

  ParameterList & params() { return params_; }
  const ParameterList & params() const { return params_; }
  const ParameterDefn * param(int index) const { return params_[index]; }
  ParameterDefn * param(int index) { return params_[index]; }
  const Type * paramType(int index) const;
  void addParam(ParameterDefn * param);
  ParameterDefn * addParam(const char * name, Type * type);

  /** Return the parameter types as a type vector. */
  TupleType * paramTypes() const;

  /** Given the name of a parameter, return the index of that parameter,
      or -1 if there is no such parameter. */
  int paramNameIndex(const char * name) const;

  // Misc methods

  /** Print an explanation as to why this function type is not singular. */
  void whyNotSingular() const;

  /** Return the name of the 'invoke' trampoline function for this function type. */
  const std::string & invokeName() const;

  // Overrides

  const llvm::Type * createIRType() const;
  const llvm::FunctionType * createIRFunctionType(
      const Type * selfType, const ParameterList & params, const Type * returnType) const;
  const llvm::Type * irEmbeddedType() const;
  const llvm::Type * irParameterType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const;
  bool isSingular() const;
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const FunctionType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Function;
  }

private:
  bool isStatic_;
  const Type * returnType_;
  ParameterDefn * selfParam_;
  ParameterList params_;
  mutable TupleType * paramTypes_;
  mutable llvm::PATypeHolder irType_;
  mutable bool isCreatingType;
  mutable std::string invokeName_;
};

// -------------------------------------------------------------------
// Type that represents a reference to a 'bound' method.

class BoundMethodType : public Type {
public:
  BoundMethodType(const FunctionType * fnType)
    : Type(BoundMethod)
      , fnType_(fnType)
      , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
    {}

  const llvm::Type * irType() const;

  /** The type of the function being pointed to. */
  const FunctionType * fnType() const { return fnType_; }

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const;
  bool isSingular() const;
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const BoundMethodType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == BoundMethod;
  }

private:
  const FunctionType * fnType_;
  mutable llvm::PATypeHolder irType_;
};

}

#endif
