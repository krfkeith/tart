/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TEMPLATE_H
#define TART_CFG_TEMPLATE_H

#ifndef TART_CFG_TYPEDEFN_H
#include "tart/CFG/TypeDefn.h"
#endif

namespace tart {

class Template;
class BindingEnv;

/// -------------------------------------------------------------------
/// Used to represent a pattern variable used within a type expressions.
class PatternVar : public DeclaredType, public Locatable {
public:

  /** Construct a type pattern variable. */
  PatternVar(const SourceLocation & loc, TypeDefn * defn,
    Scope * parentScope, TemplateSignature * temp);

  const SourceLocation & location() const { return location_; }

  /** Return the pattern variable */
  const char * name() const { return typeDefn()->name(); }

  /** Return the type of this variable, which will usually be 'Type' */
  const Type * valueType() const { return valueType_; }

  /** Set the type of values which can be bound to this variable. */
  void setValueType(Type * type) { valueType_ = type; }

  /** Return the definition in which this pattern variable is defined. */
  const Defn * templateDefn() const;

  /** Return the template in which this pattern variable is defined. */
  const TemplateSignature * getTemplate() const { return template_; }

  /** Return true if this is a type variable. */
  bool isTypeVar() const;

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
  const TemplateSignature * template_;
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

  /** Return the list of template pattern parameters. */
  const TypeList & params() const { return params_; }
  TypeList & params() { return params_; }

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

  /** Add a pattern variable. */
  PatternVar * addPatternVar(const SourceLocation & loc, const char * name, Type * type = NULL);

  /** Add a parameter which consists of a single expression pattern variable. */
  void addParameter(const SourceLocation & loc, const char * name, Type * type = NULL);

  /** Find a specialization with the specified type arguments. */
  Defn * TemplateSignature::findSpecialization(TypeVector * tv) const;

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

  TypeList params_;
  ExprList requirements_;
  PatternVarList vars_;

  typedef llvm::DenseMap<TypeVector *, Defn *, TypeVector::KeyInfo> SpecializationMap;
  SpecializationMap specializations_;
  IterableScope paramScope_;
};

/// -------------------------------------------------------------------
/// An instantiation of a template
class TemplateInstance : public GC, public Scope {
public:
  /** Construct a TemplateInstance. */
  TemplateInstance(Defn * templateDefn, TypeVector * templateArgs);

  /** The generated defn for this instance. */
  Defn * value() const { return value_; }
  void setValue(Defn * value) { value_ = value; }

  /** The original template defn for this instance. */
  Defn * templateDefn() const { return templateDefn_; }

  /** The values that are bound to pattern variables. */
  const TypeList & paramValues() const { return paramValues_; }
  TypeList & paramValues() { return paramValues_; }

  /** The template arguments for this template. */
  TypeVector * templateArgs() const { return templateArgs_; }

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
  TypeList paramValues_;            // The list of parameter values.
  OrderedSymbolTable paramDefns_;   // Symbol definitions for parameter values.
  TypeVector * templateArgs_;       // Template arguments with substitutions
  Scope * parentScope_;             // Parent scope of this definition.
  SourceLocation instantiatedFrom_; // Location which produced this instance.
};

/// -------------------------------------------------------------------
/// A list of type parameters for a type.
/// TODO: Refactor templates to use this.
class TypeParams {
public:
  TypeParams(Defn * source) : source_(source ) {}

  /** A pointer to the originating template. */
  const Defn * source() const { return source_; }
  Defn * source() { return source_; }

  /** The list of argument types that match the template signature parameters. */
  const TypeList & params() const { return params_; }
  TypeList & params() { return params_; }

  /** The type values bound to the pattern variables in the template signature. */
  //const TypeList & captures() const { return captures_; }
  //TypeList & captures() { return captures_; }

private:
  Defn * source_;                   // The originating template.
  TypeList params_;                 // The values for each template parameter.
  //TypeList captures_;               // Values captures by the pattern variables in the template.
  //OrderedSymbolTable _;   // Symbol definitions for parameter values.
};

} // namespace tart

#endif
