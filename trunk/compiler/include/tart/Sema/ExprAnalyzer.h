/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_EXPRANALYZER_H
#define TART_SEMA_EXPRANALYZER_H

#ifndef TART_SEMA_ANALYZERBASE_H
#include "tart/Sema/AnalyzerBase.h"
#endif

namespace tart {

class LValueExpr;
class CallExpr;
class SpecializeExpr;
class Stmt;
class BlockStmt;
class IfStmt;
class WhileStmt;
class DoWhileStmt;
class ForStmt;
class ForEachStmt;
class SwitchStmt;
class MatchStmt;
class MatchAsStmt;
class TryStmt;
class ThrowStmt;
class ReturnStmt;
class ReturnStmt;
class DeclStmt;
class TypeVariable;

enum AnalysisOptions {
  AO_EXPLICIT_CAST = (1<<1),        // Force a cast to the expected type, suppress warnings.
  AO_IMPLICIT_CAST = (1<<2),        // Do implicit conversion to the expected type.
  AO_IGNORE_QUALIFIERS = (1<<3),    // Ignore type qualifiers (used when comparing).
};

/// -------------------------------------------------------------------
/// Expression analyzer
class ExprAnalyzer : public AnalyzerBase {
public:
  /** Constructor. */
  ExprAnalyzer(Module * mod, Scope * activeScope, Defn * subject, FunctionDefn * currentFunction);
  ExprAnalyzer(const AnalyzerBase * parent, FunctionDefn * currentFunction);

  /** Build expression tree from AST and do all type inferencing. */
  Expr * analyze(const ASTNode * ast, QualifiedType expected, unsigned options = AO_IMPLICIT_CAST) {
    return inferTypes(subject(), reduceExpr(ast, expected), expected, options);
  }

  /** Take a reduced expression and do type inferencing. */
  Expr * inferTypes(Defn * source, Expr * expr, QualifiedType expected,
      unsigned options = AO_IMPLICIT_CAST);
  Expr * inferTypes(Expr * expr, QualifiedType expected);

  /** Build expression tree from AST. */
  Expr * reduceExpr(const ASTNode * ast, QualifiedType expected);
  Expr * reduceExprImpl(const ASTNode * ast, QualifiedType expected);

  /** Similar to reduceExpr, but applies the special name lookup rules for
      attributes. */
  Expr * reduceAttribute(const ASTNode * ast);

  /** Similar to reduceExpr, but returns a constant. */
  Expr * reduceConstantExpr(const ASTNode * ast, QualifiedType expected);

  /** Similar to reduceExpr, but returns a constant. */
  Expr * reduceTemplateArgExpr(const ASTNode * ast, bool doInference);

  /** Attempt to silently case 'in' to 'toType', using whatever means available.
      Report an error if the cast is not possible. */
  Expr * doImplicitCast(Expr * in, QualifiedType toType, unsigned options = AO_IMPLICIT_CAST);
  Expr * doBoxCast(Expr * in);
  Expr * doUnboxCast(Expr * in, QualifiedType toType);

  // Literals

  Expr * reduceNull(const ASTNode * ast);
  Expr * reduceIntegerLiteral(const ASTIntegerLiteral * ast);
  Expr * reduceFloatLiteral(const ASTFloatLiteral * ast);
  Expr * reduceDoubleLiteral(const ASTDoubleLiteral * ast);
  Expr * reduceCharLiteral(const ASTCharLiteral * ast);
  Expr * reduceStringLiteral(const ASTStringLiteral * ast);
  Expr * reduceBoolLiteral(const ASTBoolLiteral * ast);
  Expr * reduceBuiltInDefn(const ASTBuiltIn * ast);
  Expr * reduceAnonFn(const ASTFunctionDecl * ast, QualifiedType expected);

  // Identifiers

  Expr * reduceValueRef(const ASTNode * ast, bool store);
  Expr * reduceSymbolRef(const ASTNode * ast, bool store);
  Expr * reduceElementRef(const ASTOper * ast, bool store, bool allowOverloads);
  Expr * reduceGetParamPropertyValue(const SourceLocation & loc, CallExpr * call);
  Expr * reduceSetParamPropertyValue(const SourceLocation & loc, CallExpr * call, Expr * value);

  // Operators

  Expr * reduceAssign(const ASTOper * ast);
  Expr * reducePostAssign(const ASTOper * ast);
  Expr * reduceAugmentedAssign(const ASTOper * ast);
  Expr * reduceMultipleAssign(const ASTOper * ast);
  Expr * reduceLoadValue(const ASTNode * ast);
  Expr * reduceStoreValue(const SourceLocation & loc, Expr * lval, Expr * rval);
  Expr * reduceRefEqualityTest(const ASTOper * ast);
  Expr * reduceContainsTest(const ASTOper * ast);
  Expr * reduceTypeTest(const ASTOper * ast);
  Expr * reduceLogicalOper(const ASTOper * ast);
  Expr * reduceLogicalNot(const ASTOper * ast);
  Expr * reduceComplement(const ASTOper * ast);
  Expr * reduceArrayLiteral(const ASTOper * ast, QualifiedType expected);
  Expr * reduceTuple(const ASTOper * ast, QualifiedType expected);
  Expr * reduceTypeModification(const ASTOper * ast, QualifiedType expected);

  // Statements

  Expr * reduceBlockStmt(const BlockStmt * st, QualifiedType expected);
  Expr * reduceIfStmt(const IfStmt * st, QualifiedType expected);
  Expr * reduceWhileStmt(const WhileStmt * st, QualifiedType expected);
  Expr * reduceDoWhileStmt(const DoWhileStmt * st, QualifiedType expected);
  Expr * reduceForStmt(const ForStmt * st, QualifiedType expected);
  Expr * reduceForEachStmt(const ForEachStmt * st, QualifiedType expected);
  Expr * reduceSwitchStmt(const SwitchStmt * st, QualifiedType expected);
  Expr * reduceMatchStmt(const MatchStmt * st, QualifiedType expected);
  Expr * reduceMatchAsStmt(const MatchAsStmt * st, Expr * testExpr, Expr::ExprType castType,
      QualifiedType expected);
  Expr * reduceTryStmt(const TryStmt * st, QualifiedType expected);
  Expr * reduceThrowStmt(const ThrowStmt * st, QualifiedType expected);
  Expr * reduceReturnStmt(const ReturnStmt * st, QualifiedType expected);
  Expr * reduceYieldStmt(const ReturnStmt * st, QualifiedType expected);
  Expr * reduceBreakStmt(const Stmt * st, QualifiedType expected);
  Expr * reduceContinueStmt(const Stmt * st, QualifiedType expected);
  bool reduceDeclStmt(const DeclStmt * st, QualifiedType expected, ExprList & exprs);

  // Calls

  /** Transform an expression to a callable object. The 'expected'
      parameter is only used in overload selection, the actual result type
      may not actually be that type. */
  Expr * reduceCall(const ASTCall * call, QualifiedType expected);

  /** Reduce a call to an identifier to an actual call. */
  Expr * callName(const SourceLocation & loc, const ASTNode * callable, const ASTNodeList & args,
      QualifiedType expected, bool isOptional = false);

  /** Handle Argument-dependent lookup (ADL) */
  void lookupByArgType(CallExpr * call, StringRef name, const ASTNodeList & args);

  /** Handle expressions of the form "super(args)" */
  Expr * callSuper(const SourceLocation & loc, const ASTNodeList & args, QualifiedType expected);

  /** Select an overload and build the call expression node. */
  Expr * callExpr(const SourceLocation & loc, Expr * fun, const ASTNodeList & args,
      QualifiedType expected);

  /** Evaluate a call to a constructor. */
  Expr * callConstructor(const SourceLocation & loc, TypeDefn * tdef, const ASTNodeList & args);

  /** Evaluate a call to a type function. */
  Expr * callTypeFunction(SLC & loc, QualifiedType callable, const ASTNodeList & args);

  /** Add all overloaded constructors to the given call expression. */
  bool addOverloadedConstructors(SLC & loc, CallExpr * call, TypeDefn * tdef,
      const ASTNodeList & args, SpCandidate * sp);

  /** Attempt a coercive cast, that is, try to find a 'coerce' method that will convert
      to 'toType'. */
  CallExpr * tryCoerciveCast(Expr * in, const Type * toType);

  /** Evaluate the argument list. */
  bool reduceArgList(const ASTNodeList & in, CallExpr * call);

  /** Evaluate the function return type. */
  QualifiedType reduceReturnType(CallExpr * call);

  /** Given the index of an input argument, determine for each candidate
      which parameter that argument maps to, and return a set containing
      the types of those parameters. */
  QualifiedType getMappedParameterType(CallExpr * call, int index);

  /** Add an overload to a call expression.
      Parameters:
        'call' - The call expression.
        'callable' - The callable expression (function or function-typed variable.)
        'args' - the list of argument AST nodes (for computing keyword assignment.)
   */
  bool addOverload(CallExpr * call, Expr * callable, const ASTNodeList & args);

  /** Add an overload to a call expression.
      Parameters:
        'base' - the base expression (i.e. the 'self' argument) for each method.
        'methods' - the list of methods.
        'args' - the list of argument AST nodes (for computing keyword
                 assignment.)
   */
  bool addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
      const ASTNodeList & args);

  /** Version of addOverload which accepts an environment containing explicit specialization
      bindings. */
  bool addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
      const ASTNodeList & args, SpCandidate * sp);

  /** Version of addOverload which works with expressions of function type. */
  bool addOverload(CallExpr * call, Expr * callable, const FunctionType * ftype,
      const ASTNodeList & args);

  /** Version of addOverload which works with expressions of composite type that have
      a call method. */
  bool addOverloads(CallExpr * call, Expr * callable, const CompositeType * ftype,
      const ASTNodeList & args);

  /** Version of overload which works with pre-analyzed arguments. No keyword mapping
      is done, args are simply mapped 1:1 to parameters. */
  bool addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
      const ExprList & args);

  // Specializations

  /** Return either the single best specialization candidate, or NULL. */
  static Defn * findBestSpecialization(SpecializeExpr * spe);

  // Type conversions

  /** Given a type, return the coercion function to convert it to a reference type. */
  FunctionDefn * coerceToObjectFn(const Type * type);

  /** Return the function to unbox the specified type. */
  FunctionDefn * getUnboxFn(const SourceLocation & loc, const Type * toType);

  /** Return the function to downcast to the specified type. */
  FunctionDefn * getDowncastFn(const SourceLocation & loc, const Type * toType);

  /** Check to see if a call to infixLogicalOr is really a union type expression. */
  bool getUnionTypeArgs(Expr * ex, QualifiedTypeList & types);

  /** Report that there were no matching candidates. */
  void noCandidatesError(CallExpr * call, const ExprList & methods);

  /** Given a value symbol and a base expression, return an LValue reference for that symbol. */
  Expr * getLValue(SLC & loc, ValueDefn * val, Expr * base, bool store);

  /** Given an LValue, return the base expression. */
  Expr * lvalueBase(LValueExpr * lval);

  /** Set the function return type. If in a macro expansion, this will be the
      return type of the macro. */
  QualifiedType setReturnType(QualifiedType returnType);

  /** Cause a 'return' statement to instead assign to a local variable.
      Used during macro expansion. Returns previous value. */
  LValueExpr * setMacroReturnVal(LValueExpr * retVal);

  /** Flag that indicates we're inside a macro expansion. */
  bool setInMacroExpansion(bool value);

protected:
  QualifiedType returnType_;
  LValueExpr * macroReturnVal_;
  bool inMacroExpansion_;

private:
  ConstantExpr * reduceCaseValue(const ASTNode * ast, QualifiedType testType);
  LocalScope * createLocalScope(const char * scopeName);
  Defn * createLocalDefn(const ASTDecl * ast);
  Expr * createTempVar(Defn::DefnType kind, const char * name, Expr * value);
  VariableDefn * createTempVar(
      const SourceLocation & loc, Defn::DefnType kind, QualifiedType type, const char * name);
  Expr * reduceTestExpr(const ASTNode * test, LocalScope *& implicitScope, bool castToBool = true);
  Defn * astToDefn(const ASTDecl * ast);
  bool astToDefnList(const ASTVarDecl * ast, DefnList & vars);

  /** True if any catch target can catch the specified exception type */
  bool canCatch(TypeList & catchTypes, const CompositeType * exceptionType);

  /** Given an interface (which may be a template), and a concrete type, first locate the method
      in the interface, and then find the overloaded version of that method in the concrete type. */
  FunctionDefn * findInterfaceMethod(const CompositeType * type, const Type * interface,
      const char * method);
};

} // namespace tart

#endif
