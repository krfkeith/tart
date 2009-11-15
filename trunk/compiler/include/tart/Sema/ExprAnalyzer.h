/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_EXPRANALYZER_H
#define TART_SEMA_EXPRANALYZER_H

#ifndef TART_SEMA_ANALYZERBASE_H
#include "tart/Sema/AnalyzerBase.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Expression analyzer
class ExprAnalyzer : public AnalyzerBase {
public:
  /** Constructor. */
  ExprAnalyzer(Module * mod, Scope * parent, FunctionDefn * currentFunction = NULL);
  ExprAnalyzer(Module * mod, Scope * parent, Defn * subject);

  /** Build expression tree from AST and do all type inferencing. */
  Expr * analyze(const ASTNode * ast, Type * expected) {
    return inferTypes(subject(), reduceExpr(ast, expected), expected);
  }

  /** Take a reduced expression and do type inferencing. */
  static Expr * inferTypes(Defn * source, Expr * expr, Type * expected);

  /** Build expression tree from AST. */
  Expr * reduceExpr(const ASTNode * ast, Type * expected);
  Expr * reduceExprImpl(const ASTNode * ast, Type * expected);

  /** Similar to reduceExpr, but applies the special name lookup rules for
   attributes. */
  Expr * reduceAttribute(const ASTNode * ast);

  /** Similar to reduceExpr, but returns a constant. */
  ConstantExpr * reduceConstantExpr(const ASTNode * ast, Type * expected);
  Expr * reducePattern(const ASTNode * ast, TemplateSignature * tsig);

  /** Attempt to silently case 'in' to 'toType', using whatever means available.
   Report an error if the cast is not possible. */
  Expr * doImplicitCast(Expr * in, Type * toType);
  Expr * doImplicitCast(Expr * in, const TypeRef & toType);
  Expr * doUnboxCast(Expr * in, Type * toType);

  // Literals

  Expr * reduceNull(const ASTNode * ast);
  Expr * reduceIntegerLiteral(const ASTIntegerLiteral * ast);
  Expr * reduceFloatLiteral(const ASTFloatLiteral * ast);
  Expr * reduceDoubleLiteral(const ASTDoubleLiteral * ast);
  Expr * reduceCharLiteral(const ASTCharLiteral * ast);
  Expr * reduceStringLiteral(const ASTStringLiteral * ast);
  Expr * reduceBoolLiteral(const ASTBoolLiteral * ast);
  Expr * reduceBuiltInDefn(const ASTBuiltIn * ast);
  Expr * reduceAnonFn(const ASTFunctionDecl * ast);

  // Identifiers

  Expr * reduceValueRef(const ASTNode * ast, bool store);
  Expr * reduceSymbolRef(const ASTNode * ast, bool store);
  Expr * reduceElementRef(const ASTOper * ast, bool store);
  Expr * reduceLValueExpr(LValueExpr * lvalue, bool store);
  Expr * reduceGetPropertyValue(const SourceLocation & loc, Expr * basePtr, PropertyDefn * prop);
  Expr * reduceSetPropertyValue(const SourceLocation & loc, Expr * basePtr, PropertyDefn * prop,
      Expr * value);
  Expr * reduceGetParamPropertyValue(const SourceLocation & loc, CallExpr * call);
  Expr * reduceSetParamPropertyValue(const SourceLocation & loc, CallExpr * call, Expr * value);
  Expr * reducePatternVar(const ASTPatternVar * ast);

  // Operators

  Expr * reduceAssign(const ASTOper * ast);
  Expr * reducePostAssign(const ASTOper * ast);
  Expr * reduceAugmentedAssign(const ASTOper * ast);
  Expr * reduceLoadValue(const ASTNode * ast);
  Expr * reduceStoreValue(const SourceLocation & loc, Expr * lval, Expr * rval);
  Expr * reduceRefEqualityTest(const ASTOper * ast);
  Expr * reduceContainsTest(const ASTOper * ast);
  Expr * reduceTypeTest(const ASTOper * ast);
  Expr * reduceLogicalOper(const ASTOper * ast);
  Expr * reduceLogicalNot(const ASTOper * ast);
  Expr * reduceArrayLiteral(const ASTOper * ast, Type * expected);

  // Calls

  /** Transform an expression to a callable object. The 'expected'
   parameter is only used in overload selection, the actual result type
   may not actually be that type. */
  Expr * reduceCall(const ASTCall * call, Type * expected);

  /** Reduce a call to an identifier to an actual call. */
  Expr * callName(const SourceLocation & loc, const ASTNode * callable, const ASTNodeList & args,
      Type * expected, bool isOptional = false);

  /** Handle Argument-dependent lookup (ADL) */
  void lookupByArgType(CallExpr * call, const char * name, const ASTNodeList & args);

  /** Handle expressions of the form "super(args)" */
  Expr * callSuper(const SourceLocation & loc, const ASTNodeList & args, Type * expected);

  /** Select an overload and build the call expression node. */
  Expr * callExpr(const SourceLocation & loc, Expr * fun, const ASTNodeList & args, Type * expected);

  /** Evaluate a call to a constructor. */
  Expr * callConstructor(const SourceLocation & loc, TypeDefn * tdef, const ASTNodeList & args);

  /** Attempt a coercive cast, that is, try to find a 'coerce' method that will convert
   to 'toType'. */
  CallExpr * tryCoerciveCast(Expr * in, Type * toType);

  /** Evaluate the argument list. */
  bool reduceArgList(const ASTNodeList & in, CallExpr * call);

  /** Evaluate the function return type. */
  Type * reduceReturnType(CallExpr * call);

  /** Given the index of an input argument, determine for each candidate
   which parameter that argument maps to, and return a set containing
   the types of those parameters. */
  Type * getMappedParameterType(CallExpr * call, int index);

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

  /** Version of addOverload which works with variables of function type. */
  bool addOverload(CallExpr * call, LValueExpr * fn, const FunctionType * ftype,
      const ASTNodeList & args);
  // Templates

  /** Version of overload which works with pre-analyzed arguments. No keyword mapping
   is done, args are simply mapped 1:1 to parameters. */
  bool addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
      const ExprList & args);

  Expr * reduceSpecialize(const ASTSpecialize * call, Type * expected);

  /** Return either the single best specialization candidate, or NULL. */
  Defn * findBestSpecialization(SpecializeExpr * spe);

  /** Given an LValue, return the base expression. */
  Expr * lvalueBase(LValueExpr * lval);

  /** Return the function to unbox the specified type. */
  FunctionDefn * getUnboxFn(const SourceLocation & loc, Type * toType);

private:
  TemplateSignature * tsig_;
  FunctionDefn * currentFunction_;
};

} // namespace tart

#endif
