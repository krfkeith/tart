/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_FUNCTIONDEFN_H
#define TART_CFG_FUNCTIONDEFN_H

#ifndef TART_CFG_VARIABLEDEFN_H
#include "tart/CFG/VariableDefn.h"
#endif

#ifndef TART_CFG_FUNCTIONTYPE_H
#include "tart/CFG/FunctionType.h"
#endif

namespace tart {

class FunctionRegion;

/// -------------------------------------------------------------------
/// A definition of a function parameter
class ParameterDefn : public VariableDefn {
public:
  enum ParameterFlag {
    Variadic = (1<<0),      // Allows multiple values via "..."
    Reference = (1<<1),     // Value passed by reference, even if value type
    LValueParam = (1<<2),   // Allow taking address or mutating param
    KeywordOnly = (1<<3),   // A "keyword only" argument.
    ClosureEnv = (1<<4),    // A reference to a closure environment.
  };

  /** Constructor that takes a name */
  ParameterDefn(Module * m, const char * name)
    : VariableDefn(Parameter, m, name)
    , internalType_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {}

  /** Constructor that takes a name and a defn type */
  ParameterDefn(DefnType dt, Module * m, const char * name)
    : VariableDefn(dt, m, name)
    , internalType_(NULL)
    , variance_(Contravariant)
    , flags_(0)
  {}

  /** Constructor that takes an AST declaration. */
  ParameterDefn(Module * m, ASTDecl * de)
    : VariableDefn(Parameter, m, de)
    , variance_(Contravariant)
    , flags_(0)
  {}

  /** Constructor that takes an AST declaration and a defn type. */
  ParameterDefn(DefnType dt, Module * m, ASTDecl * de)
    : VariableDefn(dt, m, de)
    , variance_(Contravariant)
    , flags_(0)
  {}

  /** Constructor that takes a name and a type (for static decls.) */
  ParameterDefn(Module * m, const char * name, const Type * ty, int paramFlags,
      Expr * defaultVal = NULL)
    : VariableDefn(Parameter, m, name, defaultVal)
    , internalType_(ty)
    , variance_(Contravariant)
    , flags_(paramFlags)
  {
    setType(ty);
    assert(ty != NULL);
  }

  /** The 'internal' type is the type of the parameter as it appears within the function body,
      which may not be the same as it appears externally. */
  const Type * internalType() const { return internalType_; }
  void setInternalType(const Type * type) { internalType_ = type; }

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

  bool isReference() const { return getFlag(Reference); }
  bool isVariadic() const { return getFlag(Variadic); }
  bool isKeywordOnly() const { return getFlag(KeywordOnly); }
  bool isLValue() const { return getFlag(LValueParam); }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ParameterDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Parameter;
  }

private:
  const Type * internalType_;
  Variance variance_;
  uint32_t flags_;
};

/// -------------------------------------------------------------------
/// A definition of a type
class FunctionDefn : public ValueDefn {
public:
  typedef llvm::SmallPtrSet<FunctionDefn *, 1> FunctionSet;

  enum FunctionFlag {
    Abstract = (1<<0),          // Function is explicitly abstract.
    InterfaceMethod = (1<<1),   // Function is defined in an interface.
    Undefined = (1<<2),         // Used to undefine a method in a base class.
    Override = (1<<3),          // Function overrides definition in base class.
    Extern = (1<<4),            // Function overrides one in a base class.
    Ctor = (1<<5),              // Function is a constructor
    Final = (1<<6),             // Function cannot be overridden
    //Commutative = (1<<6),  // A function whose order of arguments can be reversed
    //Associative = (1<<7),  // A varargs function that can be combined with itself.
  };

  enum AnalysisPass {
    AttributePass,
    ParameterTypePass,
    ModifierPass,
    ControlFlowPass,
    ReturnTypePass,
    PrepConversionPass,
    MergePass,
    CompletionPass,
    ReflectionPass,
    PassCount,
  };

  typedef tart::PassMgr<AnalysisPass, PassCount> PassMgr;
  typedef PassMgr::PassSet PassSet;

  /** Constructor that takes an AST */
  FunctionDefn(DefnType dtype, Module * m, const ASTFunctionDecl * ast);

  /** Constructor that takes a name */
  FunctionDefn(DefnType dtype, Module * m, const char * name);

  /** Constructor used for static type construction */
  FunctionDefn(Module * m, const char * name, FunctionType * ty);

  /** Function type. */
  FunctionType * functionType() { return type_; }
  const FunctionType * functionType() const { return type_; }
  void setFunctionType(FunctionType * ftype) { type_ = ftype; }

  /** Function parameter definitions. */
  const ParameterList & params() const { return type_->params(); }
  ParameterList & params() { return type_->params(); }

  /** Return type. */
  const Type * returnType() const;

  /** Scope containing the parameters. */
  const IterableScope & parameterScope() const { return parameterScope_; }
  IterableScope & parameterScope() { return parameterScope_; }

  /** Function flags. */
  uint32_t flags() const { return flags_; }
  void setFlag(uint32_t flag, bool set = true) {
    if (set) {
      flags_ |= flag;
    } else {
      flags_ &= ~flag;
    }
  }

  /** List of basic blocks. */
  const BlockList & blocks() const { return blocks_; }
  BlockList & blocks() { return blocks_; }

  /** List of all local scopes. */
  const LocalScopeList & localScopes() const { return localScopes_; }
  LocalScopeList & localScopes() { return localScopes_; }

  /** Function AST. */
  const ASTFunctionDecl * functionDecl() const {
    return static_cast<const ASTFunctionDecl *>(ast_);
  }

  FunctionRegion * region() const { return region_; }
  void setRegion(FunctionRegion * region) { region_ = region; }

  /** The index into the dispatch table for the enclosing class. */
  int dispatchIndex() const { return dispatchIndex_; }
  void setDispatchIndex(int index) { dispatchIndex_ = index; }

  Intrinsic * intrinsic() const { return intrinsic_; }
  void setIntrinsic(Intrinsic * in) { intrinsic_ = in; }
  bool isIntrinsic() const { return intrinsic_ != NULL; }

  FunctionDefn * mergeTo() const { return mergeTo_; }
  void setMergeTo(FunctionDefn * f) { mergeTo_ = f; }

  /** Various function aspects. */
  bool isAbstract() const { return (flags_ & Abstract) != 0; }
  bool isInterfaceMethod() const { return (flags_ & InterfaceMethod) != 0; }
  bool isUndefined() const { return (flags_ & Undefined) != 0; }
  bool isOverride() const { return (flags_ & Override) != 0; }
  bool isExtern() const { return (flags_ & Extern) != 0; }
  bool isCtor() const { return (flags_ & Ctor) != 0; }
  bool isFinal() const { return (flags_ & Final) != 0; }

  /** True if this function has a body. */
  bool hasBody() const;

  /** Evaluate this function at compile-time. Return
      NULL if the function is not compile-time evaluable. */
  virtual Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const;

  /** The set of functions which have been overridden by this function. */
  FunctionSet & overriddenMethods() { return overriddenMethods_; }
  const FunctionSet & overriddenMethods() const { return overriddenMethods_; }

  /** True if this function is an override of 'baseFunction' */
  bool isOverrideOf(const FunctionDefn * baseFunction);

  /** True if both methods have the same signature. */
  bool hasSameSignature(const FunctionDefn * other) const;

  /** Return true if this function can override the function 'base'. */
  bool canOverride(const FunctionDefn * base) const;

  /** The current passes state. */
  const PassMgr & passes() const { return passes_; }
  PassMgr & passes() { return passes_; }

  /** Print out the basic blocks. */
  void dumpBlocks();

  // Overrides

  const std::string & linkageName() const;
  const Type * type() const;
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const FunctionDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Function || de->defnType() == Macro;
  }

private:
  FunctionType * type_;
  uint32_t flags_;
  BlockList blocks_;
  IterableScope parameterScope_;
  LocalScopeList localScopes_;
  int dispatchIndex_;
  Intrinsic * intrinsic_;
  FunctionSet overriddenMethods_;
  FunctionRegion * region_;
  PassMgr passes_;
  FunctionDefn * mergeTo_;
};

} // namespace tart

#endif
