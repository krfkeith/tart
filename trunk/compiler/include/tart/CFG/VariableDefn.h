/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_VARIABLEDEFN_H
#define TART_CFG_VARIABLEDEFN_H

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A definition of a variable
class VariableDefn : public ValueDefn {
public:
  enum AnalysisPass {
    AttributePass,
    VariableTypePass,
    InitializerPass,
    CompletionPass,
    PassCount
  };

  enum VariableFlag {
    Constant = (1<<0),          // Variable is immutable
    ThreadLocal = (1<<1),       // Variable is thread local
    Extern = (1<<2),            // Variable is external to this module
  };

  typedef tart::PassMgr<AnalysisPass, PassCount> PassMgr;
  typedef PassMgr::PassSet PassSet;

  /** Constructor that takes a name */
  VariableDefn(DefnType dtype, Module * m, const char * name, Expr * value = NULL);

  /** Constructor that takes an AST declaration. */
  VariableDefn(DefnType dtype, Module * m, const ASTDecl * de);

  /** Initial value for this variable. */
  const Expr * initValue() const { return initValue_; }
  Expr * initValue() { return initValue_; }
  void setInitValue(Expr * e) { initValue_ = e; }

  /** IR representation of this variable. */
  llvm::Value * irValue() const { return irValue_; }
  void setIRValue(llvm::Value * ir) const { irValue_ = ir; }

  /** For member variables, the index of this field within the class. */
  int memberIndex() const { return memberIndex_; }
  void setMemberIndex(int index) { memberIndex_ = index; }

  /** For member variables, the index of this field within the class. */
  int memberIndexRecursive() const { return memberIndexRecursive_; }
  void setMemberIndexRecursive(int index) { memberIndexRecursive_ = index; }

  /** Set the type of this variable. */
  void setType(const Type * ty) { type_= ty; }

  /** Variable flags. */
  uint32_t flags() const { return flags_; }
  bool getFlag(uint32_t flag) const { return (flags_ & flag) != 0; }
  void setFlag(uint32_t flag, bool set = true) {
    if (set) {
      flags_ |= flag;
    } else {
      flags_ &= ~flag;
    }
  }

  /** True if the value of this variable is always the initializer. This will always be
      true for 'let' variables except in the special case of 'let' variables that
      are of instance scope and which are initialized in the constructor. */
  bool isConstant() const { return getFlag(Constant); }
  void setIsConstant(bool isConstant) { setFlag(Constant, isConstant); }

  /** True if this is a thread-local variable. */
  bool isThreadLocal() const { return getFlag(ThreadLocal); }
  void setThreadLocal(bool threadLocal) { setFlag(ThreadLocal, threadLocal); }

  /** True if this variable is external to this module. */
  bool isExtern() const { return getFlag(Extern); }

  /** Return true if this variable represents a memory location in which its value
      is stored, false if it is a constant with no storage. */
  bool hasStorage() const;

  /** Which analysis passes are running / have run. */
  const PassMgr & passes() const { return passes_; }
  PassMgr & passes() { return passes_; }

  // Overrides

  const Type * type() const { return type_; }
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const VariableDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Let || de->defnType() == Var || de->defnType() == MacroArg;
  }

private:
  const Type * type_;
  uint32_t flags_;
  Expr * initValue_;
  mutable llvm::Value * irValue_;
  int memberIndex_;
  int memberIndexRecursive_;
  PassMgr passes_;
};

} // namespace tart

#endif // TART_CFG_VARIABLEDEFN_H
