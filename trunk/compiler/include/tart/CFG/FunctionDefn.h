/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_CFG_FUNCTIONDEFN_H
#define TART_CFG_FUNCTIONDEFN_H

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A definition of a function parameter
class ParameterDefn : public ValueDefn {
public:
  enum ParameterFlag {
    Variadic = (1<<0),    // Allows multiple values via "..."
    Reference = (1<<1),   // Value passed by reference, even if value type
    LValueParam = (1<<2), // Allow taking address or mutating param
  };
  
  /** Constructor that takes a name */
  ParameterDefn(Module * m, const char * name)
    : ValueDefn(Parameter, m, name)
    , type_(NULL)
    , defaultValue_(NULL)
    , irValue_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {}

  /** Constructor that takes a name and a defn type */
  ParameterDefn(DefnType dt, Module * m, const char * name)
    : ValueDefn(dt, m, name)
    , type_(NULL)
    , defaultValue_(NULL)
    , irValue_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {}

  /** Constructor that takes an AST declaration. */
  ParameterDefn(Module * m, ASTDecl * de)
    : ValueDefn(Parameter, m, de)
    , type_(NULL)
    , defaultValue_(NULL)
    , irValue_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {}
  
  /** Constructor that takes an AST declaration and a defn type. */
  ParameterDefn(DefnType dt, Module * m, ASTDecl * de)
    : ValueDefn(dt, m, de)
    , type_(NULL)
    , defaultValue_(NULL)
    , irValue_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {}
  
  /** Constructor that takes a name and a type (for static decls.) */
  ParameterDefn(Module * m, const char * name, Type * ty, int paramFlags, Expr * defaultVal = NULL)
    : ValueDefn(Parameter, m, name)
    , type_(ty)
    , defaultValue_(defaultVal)
    , irValue_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {
    assert(ty != NULL);
  }
  
  /** Default value for this parameter. */
  const Expr * defaultValue() const { return defaultValue_; }
  Expr * defaultValue() { return defaultValue_; }
  void setDefaultValue(Expr * e) { defaultValue_ = e; }
  
  /** Set the type of this parameter. */
  void setType(Type * ty) { type_ = ty; }

  /** IR representation of this function. */
  llvm::Value * getIRValue() const { return irValue_; }
  void setIRValue(llvm::Value * ir) { irValue_ = ir; }
  
  /** Whether this parameter is covariant, contravariant, or invariant. */
  Variance variance() const { return variance_; }
  void setVariance(Variance v) { variance_ = v; }
  
  /** Parameter flags */
  bool getFlags() const { return flags_; }
  bool getFlag(ParameterFlag fl) const { return (flags_ & fl) != 0; }
  void setFlag(ParameterFlag fl, bool enable = true) {
    if (enable) {
      flags_ |= fl;
    } else {
      flags_ &= ~fl;
    }
  }

  bool isVariadic() const { return getFlag(Variadic); }
  
  // Overrides

  Type * getType() const { return type_; }
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ParameterDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Parameter || de->defnType() == TemplateParam;
  }

private:
  Type * type_;
  Expr * defaultValue_;
  llvm::Value * irValue_;
  Variance variance_;
  uint32_t flags_;
};

/// -------------------------------------------------------------------
/// A definition of a type
class FunctionDefn : public ValueDefn {
public:
  /** Constructor that takes an AST */
  FunctionDefn(DefnType dtype, Module * m, const ASTFunctionDecl * ast)
    : ValueDefn(dtype, m, ast)
    , type_(NULL)
    , irFunction_(NULL)
    , dispatchIndex_(-1)
    , intrinsic_(NULL)
  {}

  /** Constructor that takes a name */
  FunctionDefn(DefnType dtype, Module * m, const char * name)
    : ValueDefn(dtype, m, name)
    , type_(NULL)
    , irFunction_(NULL)
    , dispatchIndex_(-1)
    , intrinsic_(NULL)
  {}

  /** Constructor used for static type construction */
  FunctionDefn(Module * m, const char * name, FunctionType * ty)
    : ValueDefn(Function, m, name)
    , type_(ty)
    , irFunction_(NULL)
    , dispatchIndex_(-1)
    , intrinsic_(NULL)
  {}

  /** Function type. */
  FunctionType * functionType() { return type_; }
  const FunctionType * functionType() const { return type_; }
  void setFunctionType(FunctionType * ftype) { type_ = ftype; }

  /** Return type type. */
  Type * returnType() const;
  
  /** Scope containing the parameters. */
  const Scope & parameterScope() const { return parameterScope_; }
  Scope & parameterScope() { return parameterScope_; }

  /** List of basic blocks. */
  const BlockList & blocks() const { return blocks_; }
  BlockList & blocks() { return blocks_; }
  
  /** List of all local scopes. */
  const LocalScopeList & localScopes() const { return localScopes_; }
  LocalScopeList & localScopes() { return localScopes_; }
  
  /** Function AST. */
  const ASTFunctionDecl * getFunctionDecl() const {
    return static_cast<const ASTFunctionDecl *>(ast);
  }
  
  /** IR representation of this function. */
  llvm::Function * irFunction() const { return irFunction_; }
  void setIRFunction(llvm::Function * ir) { irFunction_ = ir; }

  /** The index into the dispatch table for the enclosing class. */
  int dispatchIndex() const { return dispatchIndex_; }
  void setDispatchIndex(int index) { dispatchIndex_ = index; }
  
  Intrinsic * intrinsic() const { return intrinsic_; }
  void setIntrinsic(Intrinsic * in) { intrinsic_ = in; }
  bool isIntrinsic() const { return intrinsic_ != NULL; }
  
  /** True if this function has a body. */
  bool hasBody() const;
  
  /** Evaluate this function at compile-time. Return
      NULL if the function is not compile-time evaluable. */
  virtual Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const;

  /** Print out the basic blocks. */
  void dumpBlocks();

  // Overrides 

  const std::string & getLinkageName() const;
  Type * getType() const;
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const FunctionDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Function || de->defnType() == Macro;
  }
  
private:
  FunctionType * type_;
  BlockList blocks_;
  IterableScope parameterScope_;
  LocalScopeList localScopes_;
  llvm::Function * irFunction_;
  int dispatchIndex_;
  Intrinsic * intrinsic_;
};

} // namespace tart

#endif
