/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TEMPLATE_H
#define TART_CFG_TEMPLATE_H

#ifndef TART_CFG_TYPEDEFN_H
#include "tart/CFG/TypeDefn.h"
#endif

namespace tart {

class BindingEnv;

/// -------------------------------------------------------------------
/// Used to represent a pattern variable used within a type expressions.
class PatternVar : public TypeImpl, public Locatable {
public:

  /** Construct a type pattern variable. */
  PatternVar(const SourceLocation & loc, const char * name, const Type * valueType = NULL);

  /** Location where this variable was defined. */
  const SourceLocation & location() const { return location_; }

  /** The variable name. */
  const char * name() const { return name_; }

  /** Return the type of this variable, which will usually be 'Type' */
  const Type * valueType() const { return valueType_; }

  /** Set the type of values which can be bound to this variable. */
  void setValueType(Type * type) { valueType_ = type; }

  /** Return true if the specified type value can be bound to this type. */
  bool canBindTo(const Type * value) const;

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSubtype(const Type * other) const;
  void trace() const;
  bool isReferenceType() const;
  bool isSingular() const;
  void format(FormatStream & out) const;

  static inline bool classof(const PatternVar *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Pattern;
  }

private:
  const SourceLocation location_;
  const Type * valueType_;
  const char * name_;
};

typedef llvm::SmallVector<PatternVar *, 4> PatternVarList;

/// -------------------------------------------------------------------
/// Defines the parameters of a template. Also used as a record of
/// specializations of this template.
class TemplateSignature : public GC {
public:
  static TemplateSignature * get(Defn * v, Scope * parent);

  TemplateSignature(Defn * v, Scope * parentScope);

  const Defn * value() const { return value_; }

  const ASTTemplate * ast() const { return ast_; }
  void setAST(const ASTTemplate * a) { ast_ = a; }

  /** Return the list of formal template type parameters. */
  const TupleType * typeParams() const { return typeParams_; }
  void setTypeParams(const TupleType * typeParams);

  /** Return the Nth type param. */
  const TypeRef & typeParam(int index) const;

  /** Return the list of requirements. */
  const ExprList & requirements() const { return requirements_; }
  ExprList & requirements() { return requirements_; }

  /** Return the index of the specified pattern variable, or -1 if
      the variable is not defined for this template. */
  size_t getVarIndex(const PatternVar * var) const;

  /** Return the number of pattern vars. */
  size_t patternVarCount() const;

  /** Look up the specified pattern variable by name. */
  PatternVar * patternVar(const char * name) const;

  /** Look up the specified pattern variable by index. */
  PatternVar * patternVar(int index) const;

  /** Get the parameter scope. */
  const IterableScope & paramScope() const { return paramScope_; }
  IterableScope & paramScope() { return paramScope_; }

  /** Find a specialization with the specified type arguments. */
  Defn * findSpecialization(const TupleType * tv) const;

  /** Return a specialization of this template. */
  Defn * instantiate(const SourceLocation & loc, const BindingEnv & env, bool singular = false);

  /** Special version of instantiate for types. */
  Type * instantiateType(const SourceLocation & loc, const BindingEnv & env);

  /** Print the template param list */
  void format(FormatStream & out) const;

  // Overrides

  void trace() const;

private:
  Defn * value_;
  const ASTTemplate * ast_;

  const TupleType * typeParams_;
  ExprList requirements_;
  PatternVarList vars_;

  typedef llvm::DenseMap<const Type *, Defn *, Type::KeyInfo> SpecializationMap;
  SpecializationMap specializations_;
  IterableScope paramScope_;
};

/// -------------------------------------------------------------------
/// An instantiation of a template
class TemplateInstance : public GC, public Scope {
public:
  /** Construct a TemplateInstance. */
  TemplateInstance(Defn * templateDefn, const TupleType * templateArgs, TypeRefList & paramValues);

  /** The generated defn for this instance. */
  Defn * value() const { return value_; }
  void setValue(Defn * value) { value_ = value; }

  /** The original template defn for this instance. */
  Defn * templateDefn() const { return templateDefn_; }

  /** The values that are bound to pattern variables. */
  const TypeRefList & paramValues() const { return paramValues_; }

  /** The template arguments for this template. */
  const TupleType * typeArgs() const { return typeArgs_; }

  /** The module where this template was originally defined; Used for implicit imports
      within the template body. */
  Module * srcModule() const { return templateDefn_->module(); }

  /** The location from which this template was instantiated. */
  const SourceLocation & instantiatedFrom() const { return instantiatedFrom_; }
  SourceLocation & instantiatedFrom() { return instantiatedFrom_; }

  // Overrides

  bool allowOverloads() { return false; }
  Scope * parentScope() const { return parentScope_; }
  Defn * firstParamDefn() const { return paramDefns_.first(); }
  void addMember(Defn * d);
  bool lookupMember(const char * ident, DefnList & defs, bool inherit) const;
  void dumpHierarchy(bool full = true) const;
  void trace() const;
  void format(FormatStream & out) const;

private:
  Defn * value_;                    // The instantiated definition
  Defn * templateDefn_;             // The template definition from whence this came.
  TypeRefList paramValues_;         // The list of parameter values.
  OrderedSymbolTable paramDefns_;   // Symbol definitions for parameter values.
  const TupleType * typeArgs_;  // Template arguments with substitutions
  Scope * parentScope_;             // Parent scope of this definition.
  SourceLocation instantiatedFrom_; // Location which produced this instance.
};

} // namespace tart

#endif
