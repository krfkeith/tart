/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DEFN_TEMPLATE_H
#define TART_DEFN_TEMPLATE_H

#ifndef TART_DEFN_TYPEDEFN_H
#include "tart/Defn/TypeDefn.h"
#endif

#ifndef TART_TYPE_TYPEVARIABLE_H
#include "tart/Type/TypeVariable.h"
#endif

namespace tart {

class TemplateCondition;
typedef llvm::SmallVector<TemplateCondition *, 2> TemplateConditionList;

/// -------------------------------------------------------------------
/// Defines the parameters of a template. Also used as a record of
/// specializations of this template.
class Template : public GC {
public:
  enum Traits {
    Singular = (1<<0),
    NonScaffold = (1<<1),
  };

  static uint32_t expectedTraits(const Type * ty) {
    return (ty->isSingular() ? Singular : 0) | (ty->isScaffold() ? 0 : NonScaffold);
  }

  static uint32_t expectedTraits(QualifiedType ty) {
    return (ty->isSingular() ? Singular : 0) | (ty->isScaffold() ? 0 : NonScaffold);
  }

  static Template * get(Defn * v, Scope * parent);

  Template(Defn * v, Scope * parentScope);

  const Defn * value() const { return value_; }

  const ASTTemplate * ast() const { return ast_; }
  void setAST(const ASTTemplate * a) { ast_ = a; }

  /** Return the list of formal template type parameters. */
  const TupleType * typeParams() const { return typeParams_; }
  void setTypeParams(const TupleType * typeParams);

  /** Return true if the parameter at 'index' is a variadic parameter. */
  bool isVariadicParam(int index) const;

  /** Return the default values for the type parameters. */
  const QualifiedTypeList & typeParamDefaults() const { return typeParamDefaults_; }
  QualifiedTypeList & typeParamDefaults() { return typeParamDefaults_; }

  /** Return the Nth type param. */
  QualifiedType typeParam(int index) const;

  /** Return the list of requirements. */
  const TemplateConditionList & conditions() const { return conditions_; }
  TemplateConditionList & conditions() { return conditions_; }

  /** Return the number of pattern vars. */
  size_t patternVarCount() const;

  /** Look up the specified pattern variable by name. */
  const TypeVariable * patternVar(const char * name) const;

  /** Look up the specified pattern variable by index. */
  const TypeVariable * patternVar(int index) const;

  /** The minimum number of required arguments (the rest have defaults.) */
  size_t numRequiredArgs() const { return numRequiredArgs_; }
  void setNumRequiredArgs(size_t value) { numRequiredArgs_ = value; }

 /** Get the parameter scope. */
  const IterableScope & paramScope() const { return paramScope_; }
  IterableScope & paramScope() { return paramScope_; }

  /** Find a specialization with the specified type arguments. */
  Defn * findSpecialization(const TupleType * tv) const;

  /** Return a specialization of this template. */
  Defn * instantiate(const SourceLocation & loc, const QualifiedTypeVarMap & varValues,
      uint32_t expectedTraits = 0);

  /** Special version of instantiate for types. */
  const Type * instantiateType(const SourceLocation & loc, const QualifiedTypeVarMap & varValues,
      uint32_t expectedTraits = 0);

  /** Basic unification check - may produce false positives, but is cheap to run. */
  bool canUnify(const TupleType * args) const;
  bool canUnify(QualifiedType param, QualifiedType value) const;

  /** Returns true if this template has a variadic argument. */
  bool isVariadic() const { return isVariadic_; }

  /** Print the template param list */
  void format(FormatStream & out) const;

  // Overrides

  void trace() const;

private:
  Defn * value_;
  const ASTTemplate * ast_;
  const TupleType * typeParams_;
  QualifiedTypeList typeParamDefaults_;
  TemplateConditionList conditions_;
  TypeVariableList vars_;

  typedef llvm::DenseMap<const Type *, Defn *, Type::KeyInfo> SpecializationMap;
  SpecializationMap specializations_;
  IterableScope paramScope_;
  size_t numRequiredArgs_;
  bool isVariadic_;
};

/// -------------------------------------------------------------------
/// An instantiation of a template
class TemplateInstance : public GC, public Scope {
public:
  /** Construct a TemplateInstance. */
  TemplateInstance(Defn * templateDefn, const TupleType * templateArgs,
      const TupleType * typeVarValues);

  /** The generated defn for this instance. */
  Defn * value() const { return value_; }
  void setValue(Defn * value) { value_ = value; }

  /** The original template defn for this instance. */
  Defn * templateDefn() const { return templateDefn_; }

  /** The template arguments for this template. */
  const TupleType * typeArgs() const { return typeArgs_; }
  QualifiedType typeArg(int index) const;

  /** The template variable values for this template instance. */
  const TupleType * patternVarValues() const { return patternVarValues_; }

  /** The location from which this template was instantiated. */
  const SourceLocation & instantiatedFrom() const { return instantiatedFrom_; }
  SourceLocation & instantiatedFrom() { return instantiatedFrom_; }

  /** Find a more general instantiation of this template. */
  Defn * findLessSpecializedInstance();

  // Overrides

  bool allowOverloads() { return false; }
  Scope * parentScope() const { return parentScope_; }
  void addMember(Defn * d);
  bool lookupMember(StringRef ident, DefnList & defs, bool inherit) const;
  void dumpHierarchy(bool full = true) const;
  void trace() const;
  void format(FormatStream & out) const;

private:
  Defn * value_;                    // The instantiated definition
  Defn * templateDefn_;             // The template definition from whence this came.
  OrderedSymbolTable paramDefns_;   // Symbol definitions for parameter values.
  const TupleType * typeArgs_;      // Template arguments with substitutions
  const TupleType * patternVarValues_;// Values of the type variables.
  Scope * parentScope_;             // Parent scope of this definition.
  SourceLocation instantiatedFrom_; // Location which produced this instance.
  Defn * lessSpecialized_;          // A more general version of this template, or this one if none.
};

} // namespace tart

#endif
