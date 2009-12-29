/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_ANALYZERBASE_H
#define TART_SEMA_ANALYZERBASE_H

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

#ifndef TART_COMMON_AGENDA_H
#include "tart/Common/Agenda.h"
#endif

#include <llvm/ADT/SetVector.h>

namespace tart {

class Module;
class Scope;
class Expr;
class NamespaceDefn;
class FunctionDefn;
class ArrayLiteralExpr;
struct SourceLocation;

/// -------------------------------------------------------------------
/// Represents the set of possible operations that are done on a definition.
/// Since definitions are evaluated lazily, we need to ensure that the
/// appropriate analysis has been performed on the definition before the
/// task can begin. Each analysis task requires one or more analysis passes
/// to be run on the definition
enum AnalysisTask {
  Task_PrepTypeComparison,      // Prepare to compare with other types.
  Task_PrepMemberLookup,        // Prepare the definition for member lookup.
  Task_PrepConstruction,        // Analyze constructors.
  Task_PrepConversion,          // Analyze converters.
  Task_PrepEvaluation,          // Prepare for compile-time evaluation.
  Task_PrepTypeGeneration,      // Prepare to generate the low-level type.
  Task_PrepCodeGeneration,      // Prepare for code generation.
  Task_PrepReflection,          // Prepare to generate reflection data
};

/// -------------------------------------------------------------------
/// Base class of analyzers. Contains the machinery needed to do
/// qualified and unqualified name lookups.
class AnalyzerBase {
public:
  /** Constructor. */
  AnalyzerBase(Module * mod, Scope * parent, Defn * subject = NULL,
      FunctionDefn * currentFunction = NULL)
    : module(mod)
    , activeScope(parent)
    , subject_(subject)
    , currentFunction_(currentFunction)
  {}

  /** Replace the current active scope with a new scope. Returns the old scope. */
  Scope * setActiveScope(Scope * newScope) {
    Scope * prevScope = activeScope;
    activeScope = newScope;
    return prevScope;
  }

  /** Represents the current scope from which accesses are being made. Used to check
      whether private/protected variables can be seen. */
  //void setSubject(Defn * subject) { subject_ = subject; }
  Defn * subject() const { return subject_; }

  /** Represents the function currently being compiled. */
  FunctionDefn * currentFunction() const { return currentFunction_; }

  /** This method accepts an AST representing either an identifier or
      a dotted path of the form 'a.b.c'. It will attempt to resolve the
      path and return a list of matching definitions (there can be more
      than one result if the definition is overloaded).

      The method will attempt to look up the path in the following ways:

      -- In the current scope.
      -- In the scopes enclosing the current scope.
      -- In the scopes inherited by the current and enclosing scopes.
      -- By doing an implicit import from the current package.
      -- By doing an implicit import from tart.core.
      -- By doing an explicit import using an absolute package path.

      However, if 'absPath' is true, then only the absolute package path
      method will be used.
  */
  bool lookupName(ExprList & out, const ASTNode * ast, bool absPath = false);

  /** Given a list of expression, ensures that they are either all types or
      that none of them are (an error message is emitted otherwise.) If they
      are all types, then the type definitions are added to the output list. */
  static bool getTypesFromExprs(SLC & loc, ExprList & in, TypeList & out);

  /** Given a value definition, infer its type. */
  const Type * inferType(ValueDefn * valueDef);

  /** Do the requested analysis passes on the type. */
  static bool analyzeType(const Type * in, AnalysisTask pass);

  /** Do the a complete analysis of the module. */
  static bool analyzeModule(Module * mod);

  /** Do the requested analysis passes on the definition. */
  static bool analyzeDefn(Defn * in, AnalysisTask pass);

  /** Do the requested analysis passes on the type definition. */
  static bool analyzeValueDefn(ValueDefn * in, AnalysisTask pass);

  /** Do the requested analysis passes on the type definition. */
  static bool analyzeTypeDefn(TypeDefn * in, AnalysisTask pass);

  /** Given an element type, return the corresponding array type. The element
      type must already have been fully resolved. */
  static CompositeType * getArrayTypeForElement(const Type * elementType);

  /** Create an empty array literal, with elements of the specified type.
      Also add to the given module the external symbols needed to support
      construction of the array. */
  static ArrayLiteralExpr * createArrayLiteral(SLC & loc, const Type * elementType);

  /** Determine if the target is able to be accessed from the current source defn. */
  void checkAccess(const SourceLocation & loc, Defn * target);
  static void checkAccess(const SourceLocation & loc, Defn * source, Defn * target);
  static bool canAccess(Defn * source, Defn * target);

  /** Dump the current set of search scopes. */
  void dumpScopeHierarchy();

protected:
  Module * module;
  Scope * activeScope;
  Defn * subject_;
  FunctionDefn * currentFunction_;

  // Recursive name-lookup helper function
  bool lookupNameRecurse(ExprList & out, const ASTNode * ast, std::string & path, bool absPath);

  // Lookup an unqualified identifier in the current scope.
  bool lookupIdent(ExprList & out, const char * name, SLC & loc);

  // Look up a name in an explicit scope.
  bool findMemberOf(ExprList & out, Expr * context, const char * name, SLC & loc);

  // Find a name in a scope and return a list of matching expressions.
  bool findInScope(ExprList & out, const char * name, const Scope * scope, Expr * context,
      SLC & loc);

  // Special lookup function for static members of templated types.
  bool findStaticTemplateMember(ExprList & out, TypeDefn * type, const char * name, SLC & loc);

  // Special lookup function for static members of templated types.
  bool lookupTemplateMember(DefnList & out, TypeDefn * typeDef, const char * name, SLC & loc);

  // Given a list of expressions, find which ones are LValues that have template parameters,
  // and attempt to specialize those templates.
  Expr * specialize(SLC & loc, const ExprList & exprs, const ASTNodeList & args);

  // Given a list of expressions, find which ones are LValues that have template parameters,
  // and attempt to specialize those templates.
  Expr * specialize(SLC & loc, const ExprList & exprs, TupleType * tv);

  // Add a candidate to the list of specializations being considered.
  void addSpecCandidate(SLC & loc, SpCandidateSet & spcs, Expr * base, Defn * de, TupleType * args);

  // Lookup helper function that attempts to load a module from 'path'.
  bool importName(ExprList & out, const std::string & path, bool absPath, SLC & loc);

  // Create a reference to a definition.
  Expr * getDefnAsExpr(Defn * de, Expr * context, SLC & loc);

  // Given a list of definitions produced by a symbol lookup, convert
  // each definition into an expression (or in the case of an imported
  // symbol, multiple expressions) representing a reference to the definition.
  bool getDefnListAsExprList(SLC & loc, DefnList & defs, Expr * context, ExprList & out);

};

/** Class used to report what analysis tasks are in progress. */
class TaskInProgress {
public:
  TaskInProgress(Defn * defn, AnalysisTask task)
    : next_(tasks_)
    , defn_(defn)
    , task_(task)
  {
    tasks_ = this;
  }

  ~TaskInProgress() {
    tasks_ = next_;
  }

  static void report();

private:
  TaskInProgress * next_;
  Defn * defn_;
  AnalysisTask task_;

  static TaskInProgress * tasks_;
};

} // namespace tart

#endif
