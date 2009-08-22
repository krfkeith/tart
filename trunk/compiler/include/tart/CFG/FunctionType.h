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
  
// -------------------------------------------------------------------
// Type class for functions and macros.
class FunctionType : public Type {
private:
  Type * returnType_;
  ParameterDefn * selfParam_;
  ParameterList params_;
  mutable llvm::PATypeHolder irType_;
  mutable bool isCreatingType;

public:
  FunctionType(Type * rtype, ParameterList & plist);
  FunctionType(Type * rtype, ParameterDefn ** plist, size_t pcount);
  FunctionType(Type * rtype, ParameterDefn * selfParam, ParameterDefn ** plist, size_t pcount);
  
  Type * returnType() const { return returnType_; }
  void setReturnType(Type * type) { returnType_ = type; }

  const llvm::Type * getIRType() const;

  // Parameter methods

  /** The 'self' parameter of the function. */
  ParameterDefn * selfParam() const { return selfParam_; }
  void setSelfParam(ParameterDefn * self) { selfParam_ = self; }
  
  ParameterList & params() { return params_; }
  const ParameterList & params() const { return params_; }
  ParameterDefn * param(int index) { return params_[index]; }
  void addParam(ParameterDefn * param);
  ParameterDefn * addParam(const char * name, Type * type);
  
  /** Given the name of a parameter, return the index of that parameter,
      or -1 if there is no such parameter. */
  int getParamNameIndex(const char * name) const;
  
  // Misc methods
  
  /** Print an explanation as to why this function type is not singular. */
  void whyNotSingular() const;

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSubtype(const Type * other) const;
  void trace() const;
  bool isReferenceType() const;
  bool isSingular() const;
  void format(FormatStream & out) const;
  static inline bool classof(const FunctionType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Function;
  }
};

}

#endif
