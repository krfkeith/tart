/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_ANALYZERBASE_H
#define TART_SEMA_ANALYZERBASE_H

#ifndef TART_DEFN_DEFN_H
#include "tart/Defn/Defn.h"
#endif

#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"

namespace tart {

class Module;
class Scope;
class Expr;
class NamespaceDefn;
class FunctionDefn;
class ArrayLiteralExpr;
class SpCandidate;
struct SourceLocation;

typedef llvm::SmallSetVector<SpCandidate *, 8> SpCandidateSet;
typedef llvm::SmallVector<SpCandidate *, 8> SpCandidateList;

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

enum LookupOptions {
  LOOKUP_DEFAULT = 0,           // Default options
  LOOKUP_ABS_PATH = (1<<0),     // Means that the input is an absolute path
  LOOKUP_REQUIRED = (1<<1),     // This is the last resort, fail otherwise.
  LOOKUP_NO_RESOLVE = (1<<2),   // Don't try to fully resolve types.
};

/// -------------------------------------------------------------------
/// Base class of analyzers. Contains the machinery needed to do
/// qualified and unqualified name lookups.
class AnalyzerBase {
public:
  /** Constructor. */
  AnalyzerBase(Module * mod, Scope * activeScope, Defn * subject = NULL,
      FunctionDefn * currentFunction = NULL);

  Module * module() const { return module_; }

  Scope * activeScope() const { return activeScope_; }

  /** Replace the current active scope with a new scope. Returns the old scope. */
  Scope * setActiveScope(Scope * newScope) {
    Scope * prevScope = activeScope_;
    activeScope_ = newScope;
    return prevScope;
  }

  /** Represents the current scope from which accesses are being made. Used to check
      whether private/protected variables can be seen. */
  Defn * subject() const { return subject_; }
  Defn * setSubject(Defn * subject) {
    Defn * result = subject_;
    subject_ = subject;
    return result;
  }

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
  bool lookupName(ExprList & out, const ASTNode * ast, LookupOptions options = LOOKUP_DEFAULT);

  /** Given a list of expression, ensures that they are either all types or
      that none of them are (an error message is emitted otherwise.) If they
      are all types, then the type definitions are added to the output list. */
  static bool getTypesFromExprs(SLC & loc, ExprList & in, TypeList & out);

  /** Given a tuple expression which consists entirely of type literals, return a type literal
      of the tuple type. */
  static const Type * getTupleTypesFromTupleExpr(Expr * in);

  /** Given a value definition, infer its type. */
  const Type * inferType(ValueDefn * valueDef);

  /** Do the requested analysis passes on the type. */
  static bool analyzeType(const Type * in, AnalysisTask pass);
  static bool analyzeType(QualifiedType in, AnalysisTask pass);

  /** Do the a complete analysis of the module. */
  static bool analyzeModule(Module * mod);

  /** Do the requested analysis of a function. */
  static bool analyzeFunction(FunctionDefn * fn, AnalysisTask task);

  /** Do the requested analysis of a variable. */
  static bool analyzeVariable(VariableDefn * fn, AnalysisTask task);

  /** Do the requested analysis of a property. */
  static bool analyzeProperty(PropertyDefn * fn, AnalysisTask task);

  /** Do the requested analysis of a namespace. */
  static bool analyzeNamespace(NamespaceDefn * ns, AnalysisTask task);

  /** Do the requested analysis passes on the type definition. */
  static bool analyzeTypeDefn(TypeDefn * in, AnalysisTask pass);

  /** Do the requested analysis passes on the definition. */
  static bool analyzeDefn(Defn * in, AnalysisTask pass);

  /** Do all possible analysis on the definition. */
  static bool analyzeCompletely(Defn * in);

  /** Given an element type, return the corresponding array type. The element
      type must already have been fully resolved. */
  static const CompositeType * getArrayTypeForElement(QualifiedType elementType);

  /** Create an empty array literal, with elements of the specified type.
      Also add to the given module the external symbols needed to support
      construction of the array. */
  static ArrayLiteralExpr * createArrayLiteral(SLC & loc, const Type * elementType);

  /** Given an element type, return an empty array of that element type. */
  static Expr * getEmptyArrayOfElementType(const Type * elementType);

  /** Given a value type, return the MutableRef[value] type. */
  const CompositeType * getMutableRefType(const Type * valueType);

  /** Given a function type, return the 'Function' interface that corresponds to that type. */
  static const CompositeType * getFunctionInterfaceType(const FunctionType * ftype);

  /** Determine if the target is able to be accessed from the current source defn. */
  void checkAccess(const SourceLocation & loc, Defn * target);
  static void checkAccess(const SourceLocation & loc, Defn * source, Defn * target);
  static bool canAccess(Defn * source, Defn * target);

  /** Dump the current set of search scopes. */
  void dumpScopeHierarchy();

  /** Dump a given set of search scopes. */
  void dumpScopeList(const ExprList & lvals);

  /** True if tracing is enabled for this def. */
  static bool isTraceEnabled(Defn * de);

  /** Given a defn, make sure it's a template instance, and determine if there's another
      instance of the same template with more generic type parameters. */
  Defn * findLessSpecializedInstance(Defn * de);

  /** Given a parent definition, and a definition to locate, find the child of that
      parent which has the same AST, meaning that is is an instance of the same
      template. */
  Defn * findDefnByAst(Defn * parent, Defn * toFind);

protected:
  Module * module_;
  Scope * activeScope_;
  Defn * subject_;
  FunctionDefn * currentFunction_;

  enum MemberPreference {
    NO_PREFERENCE,
    PREFER_INSTANCE,
    PREFER_STATIC,
  };

  // Recursive name-lookup helper function
  bool lookupNameRecurse(ExprList & out, const ASTNode * ast, llvm::SmallString<0> & path,
      LookupOptions lookupOptions);

  // Lookup an unqualified identifier in the current scope.
  bool lookupIdent(ExprList & out, StringRef name, SLC & loc);

  // Lookup a fully-qualified identifier in the global scope.
  bool lookupQName(ExprList & out, StringRef name, SLC & loc);

  // Lookup a qualified identifier in the scope of a named module.
  bool lookupNameInModule(ExprList & out, StringRef modName, StringRef name, SLC & loc);

  // Look up a name in an explicit scope.
  bool findMemberOf(ExprList & out, Expr * context, StringRef name, SLC & loc);

  // Find a name in a scope and return a list of matching expressions.
  bool findInScope(ExprList & out, StringRef name, const Scope * scope, Expr * context,
      SLC & loc, MemberPreference pref);

  // Special lookup function for static members of templated types. Since the template
  // is never analyzed (only instances are), we need to search the ast.
  bool findStaticTemplateMember(ExprList & out, TypeDefn * type, StringRef name, SLC & loc);

  // Special lookup function for members of templated types. Since the template
  // is never analyzed (only instances are), we need to search the ast.
  bool lookupTemplateMember(DefnList & out, TypeDefn * typeDef, StringRef name, SLC & loc);

  // Given a list of expressions, find which ones are LValues that have template parameters,
  // and attempt to specialize those templates.
  Expr * specialize(SLC & loc, const ExprList & exprs, const ASTNodeList & args,
      bool inferArgTypes);

  // Given a list of expressions, find which ones are LValues that have template parameters,
  // and attempt to specialize those templates.
  Expr * specialize(SLC & loc, const ExprList & exprs, TupleType * tv);

  // Add a candidate to the list of specializations being considered.
  void addSpecCandidate(SLC & loc, SpCandidateSet & spcs, Expr * base, Defn * de, TupleType * args);

  // Lookup helper function that attempts to load a module from 'path'.
  bool importName(ExprList & out, StringRef path, bool absPath, SLC & loc);

  // Create a reference to a definition.
  Expr * getDefnAsExpr(Defn * de, Expr * context, SLC & loc);

  // Given a list of definitions produced by a symbol lookup, convert
  // each definition into an expression (or in the case of an imported
  // symbol, multiple expressions) representing a reference to the definition.
  bool getDefnListAsExprList(SLC & loc, DefnList & defs, Expr * context, ExprList & out);

  static llvm::cl::opt<std::string> traceDef_;
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
