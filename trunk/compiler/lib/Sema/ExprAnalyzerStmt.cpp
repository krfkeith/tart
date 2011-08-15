/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/Stmt.h"

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/TypeDefn.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Type/EnumType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/AmbiguousPhiType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/EvalPass.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/FinalizeTypesPass.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

namespace tart {

#define CHECK_EXPR(e) if (isErrorResult(e)) return &Expr::ErrorVal

Expr * ExprAnalyzer::reduceBlockStmt(const BlockStmt * st, const Type * expected) {
  LocalScope * blockScope = createLocalScope("block-scope");
  Scope * savedScope = setActiveScope(blockScope);

  const StmtList & stlist = st->stmts();
  size_t stcount = stlist.size();

  // Check for empty statement list.
  if (expected != NULL && stcount == 0) {
    diag.error(st) << "Empty statement list used in expression context";
    return &Expr::ErrorVal;
  }

  // Now process all of the statements
  ExprList exprs;
  for (size_t i = 0; i < stcount; ++i) {
    const Type * ex = (i == stcount - 1) ? expected : NULL;
    const Stmt * st = stlist[i];
    if (st->nodeType() == ASTNode::LocalDecl) {
      if (!reduceDeclStmt(static_cast<const DeclStmt *>(st), ex, exprs)) {
        setActiveScope(savedScope);
        return &Expr::ErrorVal;
      }
      continue;
    }

    Expr * e = reduceExpr(st, ex);
    if (ex == NULL) {
      e = inferTypes(e, NULL);
    }
    if (isErrorResult(e)) {
      setActiveScope(savedScope);
      return &Expr::ErrorVal;
    }
    exprs.push_back(e);
  }

  setActiveScope(savedScope);

  if (exprs.empty()) {
    return new SeqExpr(st->location(), blockScope, exprs, &VoidType::instance);
  } else if (exprs.size() == 1 && blockScope->count() == 0 &&
      currentFunction_->functionDecl() != NULL &&
      st != currentFunction_->functionDecl()->body()) {
    // There's only one expression in the block, so just return it.
    return exprs.front();
  } else {
    return new SeqExpr(st->location(), blockScope, exprs, exprs.back()->type());
  }
}

Expr * ExprAnalyzer::reduceIfStmt(const IfStmt * st, const Type * expected) {
  Scope * savedScope = activeScope();
  LocalScope * implicitScope = NULL;
  Expr * testExpr = reduceTestExpr(st->testExpr(), implicitScope);
  CHECK_EXPR(testExpr);

  DASSERT(testExpr->type() != NULL);
  bool hasElse = (st->elseSt() != NULL);

  // Statements used in expression context must return a value on all paths.
  if (expected != NULL && !hasElse) {
    diag.error(st) << "if-statement used in expression context must have an 'else' clause";
  }

  // Generate the contents of the 'then' block.
  // TODO: If test contains a reference to an optional type, shadow it...
  Expr * thenExpr = reduceExpr(st->thenSt(), expected);
  CHECK_EXPR(thenExpr);
  Expr * elseExpr = NULL;
  if (hasElse) {
    elseExpr = reduceExpr(st->elseSt(), expected);
    CHECK_EXPR(elseExpr);
  }

  setActiveScope(savedScope);
  IfExpr * result = new IfExpr(st->location(), implicitScope, testExpr, thenExpr, elseExpr);
  if (expected != NULL) {
    if (elseExpr == NULL) {
      diag.error(st) << "If-statement used in expression context must have an else block";
    } else {
      AmbiguousPhiType * phiType = new AmbiguousPhiType(
          expected == &AnyType::instance ? NULL : expected);
      phiType->add(thenExpr->type());
      phiType->add(elseExpr->type());
      result->setType(phiType);
    }
  }
  return result;
}

Expr * ExprAnalyzer::reduceWhileStmt(const WhileStmt * st, const Type * expected) {
  if (expected != NULL) {
    diag.error(st) << "statement cannot return a value";
  }

  // Generate the test expression.
  // TODO: If test contains a reference to an optional type, shadow it...
  Scope * savedScope = activeScope();
  LocalScope * implicitScope = NULL;
  Expr * testExpr = reduceTestExpr(st->testExpr(), implicitScope);
  CHECK_EXPR(testExpr);

  // Generate the loop body
  Expr * body = reduceExpr(st->body(), NULL);
  CHECK_EXPR(body);

  setActiveScope(savedScope);
  return new WhileExpr(Expr::While, st->location(), implicitScope, testExpr, body);
}

Expr * ExprAnalyzer::reduceDoWhileStmt(const DoWhileStmt * st, const Type * expected) {
  if (expected != NULL) {
    diag.error(st) << "statement cannot return a value";
  }
  Scope * savedScope = activeScope();

  Expr * body = reduceExpr(st->body(), NULL);
  CHECK_EXPR(body);

  LocalScope * implicitScope = NULL;
  Expr * testExpr = reduceTestExpr(st->testExpr(), implicitScope);
  CHECK_EXPR(testExpr);

  setActiveScope(savedScope);
  return new WhileExpr(Expr::DoWhile, st->location(), NULL, testExpr, body);
}

Expr * ExprAnalyzer::reduceForStmt(const ForStmt * st, const Type * expected) {
  if (expected != NULL) {
    diag.error(st) << "statement cannot return a value";
  }
  Scope * savedScope = activeScope();
  LocalScope * forScope = createLocalScope("for-scope");
  setActiveScope(forScope);

  // Evaluate the initialization expression
  const ASTNode * init = st->initExpr();
  Expr * initExpr = NULL;
  if (init != NULL) {
    if (init->nodeType() == ASTNode::Var) {
      const ASTVarDecl * initDecl = static_cast<const ASTVarDecl *>(init);
      VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
      if (!analyzeVariable(initDefn, Task_PrepTypeGeneration)) {
        return &Expr::ErrorVal;
      }

      DASSERT(initDefn->initValue() != NULL);
      initExpr = initDefn->initValue();
      initDefn->setInitValue(NULL);
      initExpr = new BinaryExpr(Expr::Prog2, initDefn->location(), initDefn->type(),
              new InitVarExpr(initDefn->location(), initDefn, initExpr),
              LValueExpr::get(initDefn->location(), NULL, initDefn));
    } else if (init->nodeType() == ASTNode::VarList) {
      // TODO: Implement multiple variable assignment here.
      const ASTVarDecl * varList = static_cast<const ASTVarDecl *>(init);
      //Expr * initExpr = ea.analyze(varList->value(), target->type());

      DASSERT(varList->value() != NULL);
      //Expr * initValue = initDefn->initValue();
      //initDefn->setInitValue(NULL);

      //currentBlock_->append(new InitVarExpr(astLoc(st), initDefn, initValue));

      DefnList vars;
      if (!astToDefnList(varList, vars)) {
        return &Expr::ErrorVal;
      }

      //VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
      DFAIL("Implement");
    }
  }

  // Evaluate the test expression
  Expr * testExpr = NULL;
  if (st->testExpr() != NULL) {
    testExpr = inferTypes(reduceExpr(st->testExpr(), &BoolType::instance), &BoolType::instance);
  }

  Expr * incrExpr = NULL;
  if (st->incrExpr() != NULL) {
    incrExpr = inferTypes(reduceExpr(st->incrExpr(), NULL), NULL);
    if (incrExpr != NULL && incrExpr->isSideEffectFree()) {
      diag.warn(incrExpr) << "Loop increment expression has no effect";
    }
  }

  // Generate the loop body
  Expr * body = reduceExpr(st->body(), NULL);
  CHECK_EXPR(body);

  setActiveScope(savedScope);
  return new ForExpr(st->location(), forScope, initExpr, testExpr, incrExpr, body);
}

Expr * ExprAnalyzer::reduceForEachStmt(const ForEachStmt * st, const Type * expected) {
  if (expected != NULL) {
    diag.error(st) << "statement cannot return a value";
  }
  Scope * savedScope = activeScope();
  LocalScope * forScope = createLocalScope("for-scope");
  setActiveScope(forScope);

  SourceLocation stLoc = st->location();
  SourceLocation iterLoc = st->iterExpr()->location();

  ForEachExpr * foreach = new ForEachExpr(stLoc, forScope);

  Expr * iteratorExpr = inferTypes(reduceExpr(st->iterExpr(), NULL), NULL);
  if (isErrorResult(iteratorExpr)) {
    return &Expr::ErrorVal;
  }

  if (!isa<CompositeType>(iteratorExpr->type())) {
    diag.error(iteratorExpr) << "Invalid iterator type: " << iteratorExpr->type();
    return &Expr::ErrorVal;
  }

  const CompositeType * iterType = static_cast<const CompositeType *>(iteratorExpr->type());
  AnalyzerBase::analyzeType(iterType, Task_PrepMemberLookup);
  FunctionDefn * nextFn = findInterfaceMethod(iterType, Builtins::typeIterator, "next");
  if (nextFn == NULL) {
    // If it's not an Iterator, see if it's an Iterable.
    FunctionDefn * iterate = findInterfaceMethod(iterType, Builtins::typeIterable, "iterate");
    if (iterate == NULL) {
      diag.error(iteratorExpr) << "Invalid iterator type: " << iteratorExpr->type();
      return &Expr::ErrorVal;
    }

    LValueExpr * iterMethod = LValueExpr::get(iterLoc, iteratorExpr, iterate);
    iteratorExpr = inferTypes(callExpr(iterLoc, iterMethod, ASTNodeList(), NULL), NULL);
    if (iteratorExpr == NULL) {
      return &Expr::ErrorVal;
    }

    if (!isa<CompositeType>(iteratorExpr->type())) {
      diag.error(iteratorExpr) << "Invalid iterator type: " << iteratorExpr->type();
      return &Expr::ErrorVal;
    }

    iterType = static_cast<const CompositeType *>(iteratorExpr->type());
    AnalyzerBase::analyzeType(iterType, Task_PrepMemberLookup);
    nextFn = findInterfaceMethod(iterType, Builtins::typeIterator, "next");
    if (nextFn == NULL) {
      diag.error(iteratorExpr) << "Invalid iterator type: " << iteratorExpr->type();
      return &Expr::ErrorVal;
    }
  }

  // Create a variable to hold the iterator - we need this to ensure that the garbage
  // collector doesn't free the iterator before we're done.
  VariableDefn * iteratorVar = createTempVar(
      iteratorExpr->location(), Defn::Let, iteratorExpr->type(), "foreach.iter");
  foreach->setIterator(new InitVarExpr(iteratorVar->location(), iteratorVar, iteratorExpr));

  // The list of expressions to be evaluated at the start of each iteration.
  ExprList loopExprs;
  LValueExpr * iteratorValue = LValueExpr::get(iteratorVar->location(), NULL, iteratorVar);
  LValueExpr * nextFnExpr = LValueExpr::get(stLoc, iteratorValue, nextFn);
  Expr * nextCall = inferTypes(callExpr(iteratorExpr->location(), nextFnExpr, ASTNodeList(), NULL),
      NULL);
  CHECK_EXPR(nextCall);

  Expr * nextValueExpr = SharedValueExpr::get(nextCall);
  foreach->setNext(nextValueExpr);

  const UnionType * utype = dyn_cast<UnionType>(nextCall->type());
  DASSERT(utype != NULL);
  DASSERT(utype->members().size() == 2);
  const Type * iterVarType;
  for (TupleType::const_iterator it = utype->members().begin(); it != utype->members().end();
      ++it) {
    const Type * ty = *it;
    if (!ty->isVoidType()) {
      iterVarType = ty;
    }
  }

  Expr * testExpr = new InstanceOfExpr(stLoc, nextValueExpr, &VoidType::instance);
  foreach->setTest(testExpr);
  Expr * iterValue = new CastExpr(Expr::UnionMemberCast, stLoc, iterVarType, nextValueExpr);

  // Assign the next value to the iteration variable.
  if (st->loopVars()->nodeType() == ASTNode::Var) {
    const ASTVarDecl * initDecl = static_cast<const ASTVarDecl *>(st->loopVars());
    VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
    if (initDefn->type() == NULL) {
      initDefn->setType(iterVarType);
    }

    if (!analyzeVariable(initDefn, Task_PrepTypeGeneration)) {
      return &Expr::ErrorVal;
    }

    foreach->assigns().push_back(new InitVarExpr(stLoc, initDefn, iterValue));
  } else if (st->loopVars()->nodeType() == ASTNode::VarList) {
    const ASTVarDecl * varList = static_cast<const ASTVarDecl *>(st->loopVars());
    DefnList vars;
    if (!astToDefnList(varList, vars)) {
      return &Expr::ErrorVal;
    }

    if (const TupleType * tt = dyn_cast<TupleType>(iterVarType)) {
      if (tt->size() != vars.size()) {
        diag.error(st) << "Incorrect number of iteration variables.";
        return &Expr::ErrorVal;
      }

      size_t numVars = tt->size();
      DASSERT(iterValue->isSingular());
      iterValue = SharedValueExpr::get(iterValue);
      for (size_t i = 0; i < numVars; ++i) {
        VariableDefn * var = cast<VariableDefn>(vars[i]);
        const Type * elementType = tt->member(i);
        if (var->type() == NULL) {
          var->setType(elementType);
        }

        if (!analyzeVariable(var, Task_PrepTypeGeneration)) {
          return &Expr::ErrorVal;
        }

        Expr * elementVal = new BinaryExpr(Expr::ElementRef, iterValue->location(), elementType,
            iterValue, ConstantInteger::getUInt32(i));

        elementVal = var->type()->implicitCast(stLoc, elementVal);
        if (elementVal != NULL) {
          foreach->assigns().push_back(new InitVarExpr(stLoc, var, elementVal));
        }
      }
    } else {
      diag.error(st) << "Iterator type incompatible with multiple iteration variables.";
      return &Expr::ErrorVal;
    }
  } else {
    DFAIL("Invalid loop variable");
  }

  Expr * body = reduceExpr(st->body(), NULL);
  CHECK_EXPR(body);
  foreach->setBody(body);

  setActiveScope(savedScope);
  return foreach;
}

Expr * ExprAnalyzer::reduceSwitchStmt(const SwitchStmt * st, const Type * expected) {
  Scope * savedScope = activeScope();
  Scope * caseValScope = activeScope();
  LocalScope * implicitScope = NULL;
  Expr * testExpr = reduceTestExpr(st->testExpr(), implicitScope, false);
  DASSERT(implicitScope == NULL);
  CHECK_EXPR(testExpr);
  DASSERT(testExpr->type() != NULL);

  // The switch statement
  SwitchExpr * swe = new SwitchExpr(st->location(), testExpr);

  const Type * testType = testExpr->type();
  if (const EnumType * etype = dyn_cast<EnumType>(testType)) {
    // If it's an enum, allow unqualified enum constants to be used.
    caseValScope = new DelegatingScope(
        const_cast<IterableScope *>(etype->memberScope()), activeScope());
  } else if (testType == Builtins::typeString.get()) {
    // Search for an infixEQ function in the defining scope.
    // TODO: Extend this to other kinds of types, as long as they have constant literals.
    DefnList defns;
    if (TypeDefn * tdef = testType->typeDefn()) {
      if (tdef->definingScope() != NULL) {
        tdef->definingScope()->lookupMember("infixEqual", defns, false);
      }
    }

    FunctionDefn * equalityTest = NULL;
    for (DefnList::const_iterator it = defns.begin(); it != defns.end(); ++it) {
      if (FunctionDefn * fn = dyn_cast<FunctionDefn>(*it)) {
        analyzeFunction(fn, Task_PrepTypeComparison);
        // TODO: Handle the case of multiple overloaded infix equality operators.
        DASSERT(fn->functionType()->selfParam() == NULL);
        DASSERT(fn->functionType()->params().size() == 2);
        DASSERT(equalityTest == NULL);
        equalityTest = fn;
      }
    }

    if (equalityTest == NULL) {
      diag.error(st) << "Invalid expression type for switch statement: " << testType;
      return &Expr::ErrorVal;
    }

    swe->setEqualityTestFn(equalityTest);
  } else if (testType->isIntType()) {
    if (testType->isUnsizedIntType()) {
      testType = static_cast<const UnsizedIntType *>(testType)->fixIntSize(false);
    }
  } else {
    diag.error(st) << "Invalid expression type for switch statement: " << testType;
  }

  AmbiguousPhiType * phiType = NULL;
  if (expected != NULL) {
    phiType = new AmbiguousPhiType(expected == &AnyType::instance ? NULL : expected);
    swe->setType(phiType);
  }

  const StmtList & cases = st->caseList();
  for (StmtList::const_iterator it = cases.begin(); it != cases.end(); ++it) {
    if ((*it)->nodeType() == ASTNode::Case) {
      const CaseStmt * cst = static_cast<const CaseStmt *>(*it);
      ExprList caseVals;
      Scope * saveCaseScope = setActiveScope(caseValScope);
      for (ASTNodeList::const_iterator it = cst->caseExprs().begin(); it != cst->caseExprs().end();
          ++it) {
        ConstantExpr * caseVal = reduceCaseValue(*it, testType);
        if (caseVal != NULL) {
          caseVals.push_back(caseVal);
        }
      }
      setActiveScope(saveCaseScope);

      // Build the body of the case.
      Expr * body = reduceExpr(cst->body(), expected);
      if (!isErrorResult(body) && !caseVals.empty()) {
        CaseExpr * ce = new CaseExpr(st->location(), caseVals, body);
        if (phiType != NULL && ce->type() != NULL) {
          phiType->add(ce->type());
        }
        swe->appendArg(ce);
      }
    } else if ((*it)->nodeType() == ASTNode::Block) {
      if (swe->elseCase() != NULL) {
        diag.error(st) << "Only one default case is allowed.";
      } else {
        Expr * defaultCase = reduceExpr(*it, expected);
        CHECK_EXPR(defaultCase);
        if (phiType != NULL && defaultCase->type() != NULL) {
          phiType->add(defaultCase->type());
        }
        swe->setElseCase(defaultCase);
      }
    } else {
      DFAIL("Bad case statement");
    }
  }

  if (phiType != NULL && swe->elseCase() == NULL) {
    diag.error(swe) << "Switch statement used in expression context must have an 'else' clause";
  }

  // Check for duplicate case value in switch statement.
  ConstantExprList usedCaseVals;
  for (SwitchExpr::const_iterator it = swe->begin(); it != swe->end(); ++it) {
    CaseExpr * ce = cast<CaseExpr>(*it);
    for (ExprList::iterator ci = ce->begin(); ci != ce->end(); ++ci) {
      ConstantExpr * caseVal = cast<ConstantExpr>(*ci);
      for (ConstantExprList::iterator ce = usedCaseVals.begin(); ce != usedCaseVals.end(); ++ce) {
        if (caseVal->isEqual(*ce)) {
          diag.error(caseVal) << "Duplicate case value in switch: " << caseVal;
        }
      }
      usedCaseVals.push_back(caseVal);
    }
  }

  setActiveScope(savedScope);
  return swe;
}

ConstantExpr * ExprAnalyzer::reduceCaseValue(const ASTNode * ast, const Type * testType) {
  Expr * caseVal = reduceExpr(ast, testType);
  Expr * result = NULL;
  ConversionRank rank = TypeConversion::convert(caseVal, testType, &result);
  if (rank == Incompatible) {
    diag.error(ast) << "Case value type '" << caseVal->type()
        << " is incompatible with switch expression type '" << testType << "'";
    return NULL;
  } else if (rank == Truncation) {
    diag.error(ast) << "Case value '" << caseVal << " can never equal switch expression of type '"
        << testType << "'";
  }

  result = EvalPass::eval(module_, result, false);
  if (result == NULL) {
    return NULL;
  }

  return cast<ConstantExpr>(result);
}

Expr * ExprAnalyzer::reduceMatchStmt(const MatchStmt * st, const Type * expected) {
  Scope * savedScope = activeScope();
  // TODO - I don't think reduceTestExpr is correct here.
  LocalScope * implicitScope = NULL;
  Expr * testExpr = reduceTestExpr(st->testExpr(), implicitScope, false);
  DASSERT(implicitScope == NULL);
  if (isErrorResult(testExpr)) {
    return &Expr::ErrorVal;
  }

  // TODO: There are lots of optimizations that could be done here.
  const Type * fromType = testExpr->type();
  Expr::ExprType castType = Expr::BitCast;
  if (fromType->isReferenceType()) {
    // For reference types, create a shared value so we don't re-evaluate the
    // test expression for each case.
    testExpr = new SharedValueExpr(testExpr);
  } else if (isa<UnionType>(fromType)) {
    // Because of the way union type tests work, we need a temporary variable to hold
    // the union value, so that we can take its address.
    if (implicitScope == NULL) {
      implicitScope = createLocalScope("test-scope");
    }

    testExpr = new SharedValueExpr(createTempVar(Defn::Let, "match.value", testExpr));
    castType = Expr::UnionMemberCast;
  } else {
    diag.warn(testExpr) << "Match expression's type is already known: " << fromType;
  }

  MatchExpr * me = new MatchExpr(st->location(), implicitScope, testExpr);

  AmbiguousPhiType * phiType = NULL;
  if (expected != NULL) {
    phiType = new AmbiguousPhiType(expected == &AnyType::instance ? NULL : expected);
    me->setType(phiType);
  }

  const StmtList & cases = st->caseList();
  for (StmtList::const_iterator it = cases.begin(); it != cases.end(); ++it) {
    const Stmt * s = *it;
    if (s->nodeType() == ASTNode::MatchAs) {
      Expr * asExpr = reduceMatchAsStmt(
          static_cast<const MatchAsStmt *>(s), testExpr, castType, expected);
      if (!isErrorResult(asExpr)) {
        if (phiType != NULL && asExpr->type() != NULL) {
          phiType->add(asExpr->type());
        }
        me->appendArg(asExpr);
      }
    } else if (s->nodeType() == ASTNode::Block) {
      if (me->elseCase() != NULL) {
        diag.error(st) << "Only one default case is allowed.";
      } else {
        Expr * defaultCase = reduceExpr(s, expected);
        CHECK_EXPR(defaultCase);
        if (phiType != NULL && defaultCase->type() != NULL) {
          phiType->add(defaultCase->type());
        }
        me->setElseCase(defaultCase);
      }
    } else {
      DFAIL("Bad case statement");
    }
  }

  llvm::SmallSet<const Type *, 16> typesSeen;
  for (MatchExpr::const_iterator it = me->begin(); it != me->end(); ++it) {
    MatchAsExpr * mae = cast<MatchAsExpr>(*it);
    const Type * type = mae->init()->type();
    if (typesSeen.count(type)) {
      diag.error(mae) << "Duplicate type test '" << type << "'.";
    }
    typesSeen.insert(type);
  }

  setActiveScope(savedScope);
  return me;
}

Expr * ExprAnalyzer::reduceMatchAsStmt(const MatchAsStmt * st, Expr * testExpr,
    Expr::ExprType castType, const Type * expected) {
  const ASTDecl * asDecl = st->asDecl();
  SourceLocation stLoc = asDecl->location();
  LocalScope * caseScope = createLocalScope("as-scope");
  Scope * savedScope = setActiveScope(caseScope);

  Defn * asDefn = astToDefn(asDecl);
  if (asDefn == NULL || !analyzeCompletely(asDefn)) {
    return NULL;
  }

  VariableDefn * asValueDefn = cast<VariableDefn>(asDefn);
  const Type * toType = asValueDefn->type();
  if (!analyzeType(toType, Task_PrepTypeComparison)) {
    return NULL;
  }

  // Set up the type test.
  Expr * typeTestExpr = new InstanceOfExpr(stLoc, testExpr, toType);

  // Set up the variable
  Expr * asValueExpr = new CastExpr(castType, stLoc, toType, testExpr);
  Expr * initVarExpr = new InitVarExpr(stLoc, asValueDefn, asValueExpr);

  // Build the body of the case.
  Expr * body = reduceExpr(st->body(), expected);
  CHECK_EXPR(body);

  setActiveScope(savedScope);
  return new MatchAsExpr(st->location(), caseScope, typeTestExpr, initVarExpr, body);
}

Expr * ExprAnalyzer::reduceTryStmt(const TryStmt * st, const Type * expected) {

  // Generate the code inside the body of the try block.
  Expr * body = reduceExpr(st->body(), st->elseSt() != NULL ? expected : NULL);
  CHECK_EXPR(body);

  TryExpr * te = new TryExpr(st->location(), body);

  // Create the 'catch' statement blocks.
  const StmtList & catchList = st->catchList();
  if (!catchList.empty() || st->finallySt()) {
    TypeList catchTypes;

    // Generate the catch cases
    for (StmtList::const_iterator it = catchList.begin(); it != catchList.end(); ++it) {
      CatchStmt * cst = static_cast<CatchStmt*>(*it);
      const SourceLocation loc = cst->location();

      if (cst->exceptDecl() == NULL) {
        // 'catch-all' block.
        body = reduceExpr(cst->body(), expected);
        CHECK_EXPR(body);
        te->appendArg(new CatchExpr(cst->location(), NULL, NULL, body));
        continue;
      }

      // Create a local scope in which the exception expression will be defined.
      LocalScope * catchScope = createLocalScope("catch-scope");

      // Define the exception variable in the catch scope.
      ASTDecl * exceptDecl = cst->exceptDecl();
      VariableDefn * exceptDefn = cast<VariableDefn>(ScopeBuilder::createLocalDefn(catchScope,
          currentFunction_, exceptDecl));
      VarAnalyzer va(exceptDefn, activeScope_, module_, subject_, currentFunction_);
      if (!va.analyze(Task_PrepCodeGeneration)) {
        return &Expr::ErrorVal;
      }

      // Get the exception type and determine if it is valid.
      const CompositeType * exceptType = dyn_cast<CompositeType>(dealias(exceptDefn->type()));
      if (isErrorResult(exceptType)) {
        continue;
      }

      // Analyze the exception type definition
      AnalyzerBase::analyzeType(exceptType, Task_PrepTypeComparison);
      if (exceptType == NULL || !exceptType->isSubclassOf(Builtins::typeThrowable.get())) {
        diag.fatal(exceptDecl) << "'" << exceptDecl << "' is not a subtype of Throwable";
        return &Expr::ErrorVal;
      }

      // See if any of the previous exceptions are superclasses of this one.
      if (canCatch(catchTypes, exceptType)) {
        diag.warn(loc) << "Exception handler for type " << exceptType << " can never be reached";
        continue;
      }

      exceptDefn->setType(exceptType);
      module()->addSymbol(exceptDefn);
      catchTypes.push_back(const_cast<CompositeType *>(exceptType));

      // Make the catch scope the current scope for generating the block contents.

      // Generate the catch body
      Scope * savedScope = setActiveScope(catchScope);
      body = reduceExpr(cst->body(), expected);
      CHECK_EXPR(body);

      te->appendArg(new CatchExpr(cst->location(), catchScope, exceptDefn, body));
      setActiveScope(savedScope);
    }
  }

  // Generate the 'else' statement.
  if (st->elseSt() != NULL) {
    // Generate the 'else' statement.
    Expr * ee = reduceExpr(st->elseSt(), expected);
    CHECK_EXPR(ee);
    te->setElseBlock(ee);
  }

  // Generate the finally block.
  if (st->finallySt() != NULL) {
    Expr * fe = reduceExpr(st->finallySt(), NULL);
    CHECK_EXPR(fe);
    te->setFinallyBlock(fe);
  }

  return te;
}

bool ExprAnalyzer::canCatch(TypeList & catchTypes, const CompositeType * exceptionType) {
  for (TypeList::iterator it = catchTypes.begin(); it != catchTypes.end(); ++it) {
    if (exceptionType->isSubclassOf(cast<CompositeType>(*it))) {
      return true;
    }
  }

  return false;
}

Expr * ExprAnalyzer::reduceThrowStmt(const ThrowStmt * st, const Type * expected) {
  Expr * resultVal = NULL;
  if (st->value() != NULL) {
    resultVal = inferTypes(reduceExpr(st->value(), Builtins::typeThrowable),
        Builtins::typeThrowable);
    CHECK_EXPR(resultVal);
  }

  return new ThrowExpr(st->location(), resultVal);
}

Expr * ExprAnalyzer::reduceReturnStmt(const ReturnStmt * st, const Type * expected) {
  DASSERT(returnType_ != NULL);
  Expr * resultVal = NULL;
  if (st->value() != NULL) {
    //if (funcDef->isGenerator()) {
    //  diag.fatal(astLoc(retSt),
    //      "Return value not allowed in generator function");
    //}

    analyzeType(returnType_, Task_PrepTypeComparison);
    resultVal = inferTypes(reduceExpr(st->value(), returnType_), returnType_);
    CHECK_EXPR(resultVal);

    // If the return value type is an unsized int, and there's no explicit return
    // type declared, then choose an integer type.
    const Type * exprType = resultVal->type();
    if (exprType->isUnsizedIntType() && returnType_ == NULL) {
      DFAIL("Obsolete?");
      if (TypeConversion::check(resultVal, &Int32Type::instance) >= ExactConversion) {
        resultVal->setType(&Int32Type::instance);
      } else if (TypeConversion::check(resultVal, &Int64Type::instance) >= ExactConversion) {
        resultVal->setType(&Int64Type::instance);
      }
    }

    if (returnType_ != NULL) {
      analyzeType(exprType, Task_PrepTypeComparison);
      resultVal = doImplicitCast(resultVal, returnType_);
    }
  } else if (returnType_->typeClass() == Type::Union) {
    // Converting a void to a union.
    const UnionType * utype = static_cast<const UnionType *>(returnType_);
    if (utype->hasVoidType()) {
      int typeIndex = utype->getTypeIndex(&VoidType::instance);
      CastExpr * voidValue = new CastExpr(Expr::UnionCtorCast, st->location(), utype,
          ConstantNull::get(st->location(), &VoidType::instance));
      voidValue->setTypeIndex(typeIndex);
      resultVal = voidValue;
    } else {
      diag.error(st) << "Return value required for non-void function";
    }
  } else if (!returnType_->isVoidType()) {
    diag.error(st) << "Return value required for non-void function";
  }

  if (inMacroExpansion_) {
    // We are inside a macro expansion, which means that 'return' doesn't
    // actually return, it assigns to the macro result and then branches.
    if (macroReturnVal_ != NULL) {
      // Do the assignment and branch to the macro exit.
      DASSERT(returnType_->isEqual(macroReturnVal_->type()));
      return new BinaryExpr(Expr::Prog2, st->location(), &VoidType::instance,
          new AssignmentExpr(st->location(), macroReturnVal_, resultVal),
          new BranchExpr(Expr::LocalReturn, st->location()));
    } else {
      // Simply return, don't set the return value.
      return new BranchExpr(Expr::LocalReturn, st->location());
    }
  }

  return new ReturnExpr(Expr::Return, st->location(), resultVal);
}

Expr * ExprAnalyzer::reduceYieldStmt(const ReturnStmt * st, const Type * expected) {
  DFAIL("Unimplemented");
  return NULL;
}

Expr * ExprAnalyzer::reduceBreakStmt(const Stmt * st, const Type * expected) {
  return new BranchExpr(Expr::Break, st->location());
}

Expr * ExprAnalyzer::reduceContinueStmt(const Stmt * st, const Type * expected) {
  return new BranchExpr(Expr::Continue, st->location());
}

bool ExprAnalyzer::reduceDeclStmt(const DeclStmt * st, const Type * expected, ExprList & exprs) {
  // Handle multiple vars.
  if (st->decl()->nodeType() == ASTNode::VarList) {
    DefnList vars;
    const ASTVarDecl * varList = static_cast<const ASTVarDecl *>(st->decl());
    if (!astToDefnList(varList, vars)) {
      return false;
    }

    // Gather up the types of each var. If a var has no type, then use the 'any' type.
    ConstTypeList varTypes;
    for (DefnList::const_iterator it = vars.begin(); it != vars.end(); ++it) {
      VariableDefn * var = static_cast<VariableDefn *>(*it);
      const ASTVarDecl * varDecl = static_cast<const ASTVarDecl *>(var->ast());
      DASSERT(varDecl->value() == NULL);
      if (varDecl->type() != NULL) {
        VarAnalyzer va(var, activeScope(), module(), currentFunction_, currentFunction_);
        if (!va.analyze(Task_PrepTypeComparison)) {
          return false;
        }

        varTypes.push_back(var->type());
      } else {
        varTypes.push_back(&AnyType::instance);
      }
    }

    if (varList->value() != NULL) {
      const TupleType * tt = TupleType::get(varTypes.begin(), varTypes.end());
      Expr * initExpr = inferTypes(analyze(varList->value(), tt), tt);
      if (initExpr == NULL) {
        return false;
      }

      DASSERT_OBJ(initExpr->canonicalType()->typeClass() == Type::Tuple, initExpr);
      initExpr = new SharedValueExpr(initExpr);

      if (const TupleType * ttActual = dyn_cast_or_null<TupleType>(initExpr->type())) {
        tt = ttActual;
      }

      // Now extract members from the tuple and assign to individual vars.
      int memberIndex = 0;
      for (DefnList::const_iterator it = vars.begin(); it != vars.end(); ++it, ++memberIndex) {
        VariableDefn * var = static_cast<VariableDefn *>(*it);
        Expr * initVal = new BinaryExpr(Expr::ElementRef, var->ast()->location(), tt->member(
            memberIndex), initExpr, ConstantInteger::getUInt32(memberIndex));
        exprs.push_back(new InitVarExpr(st->location(), var, initVal));
        if (var->type() == NULL) {
          DASSERT(initVal->canonicalType() != &AnyType::instance);
          var->setType(initVal->canonicalType());
        }
      }
    }

    return true;
  }

  Defn * de = astToDefn(st->decl());
  VariableDefn * var = cast<VariableDefn>(de);
  VarAnalyzer va(var, activeScope(), module(), currentFunction_, currentFunction_);
  if (!va.analyze(Task_PrepConstruction)) {
    return false;
  }

  DASSERT_OBJ(var->isSingular(), var);
  if (var->initValue() != NULL) {
//    Expr * initVal = MacroExpansionPass::run(*this, var->initValue());
//    if (!isErrorResult(initVal)) {
//    }
    Expr * initVal = var->initValue();
    var->setInitValue(NULL);
    exprs.push_back(new InitVarExpr(st->location(), var, initVal));
  }
  return true;
}

Expr * ExprAnalyzer::reduceTestExpr(const ASTNode * test, LocalScope *& implicitScope,
    bool castToBool) {
  Expr * testExpr;

  if (const ASTDecl * testDecl = dyn_cast<ASTDecl>(test)) {
    LocalScope * testScope = createLocalScope("test-scope");
    setActiveScope(testScope);

    Defn * testDefn = astToDefn(testDecl);
    if (testDefn == NULL || !analyzeDefn(testDefn, Task_PrepConstruction)) {
      return NULL;
    }

    VariableDefn * testVar = cast<VariableDefn>(testDefn);
    const Type * varType = testVar->type();
    Expr * initValue = testVar->initValue();
    DASSERT(initValue != NULL);
    testVar->setInitValue(NULL);

    Expr * varValue = LValueExpr::get(test->location(), NULL, testVar);
    Expr * initExpr = new InitVarExpr(test->location(), testVar, initValue);
    implicitScope = testScope;

    if (castToBool) {
      if (const UnionType * ut = dyn_cast<UnionType>(varType)) {
        if (ut->isSingleNullableType()) {
          testVar->setType(ut->getFirstNonVoidType());
          Expr * cmpExpr = new CompareExpr(
              test->location(), llvm::CmpInst::ICMP_NE, varValue,
              ConstantNull::get(test->location(), testVar->type()));
          return new BinaryExpr(
              Expr::Prog2, test->location(), &BoolType::instance, initExpr, cmpExpr);
        }
      }
    }

    testExpr = new BinaryExpr(
            Expr::Prog2, test->location(), varValue->type(), initExpr, varValue);
  } else {
    testExpr = reduceExpr(test, castToBool ? &BoolType::instance : NULL);
    if (isErrorResult(testExpr)) {
      return &Expr::ErrorVal;
    }

    BindingEnv env;
    if (!testExpr->isSingular()) {
      testExpr = TypeInferencePass::run(module_, testExpr, env, &BoolType::instance, false);
    }

    testExpr = FinalizeTypesPass::run(currentFunction_, testExpr, env);
#if 0
    testExpr = MacroExpansionPass::run(*this, testExpr);
#endif
    DASSERT_OBJ(testExpr->isSingular(), testExpr);
  }

  if (isErrorResult(testExpr)) {
    return &Expr::ErrorVal;
  }

  if (testExpr->type()->isVoidType()) {
    diag.error(test) << "invalid use of void return result: " << test;
    return &Expr::ErrorVal;
  }

  if (!castToBool) {
    return testExpr;
  }

  // Compare reference type with null.
  if (testExpr->type()->isReferenceType()) {
    return new CompareExpr(test->location(), llvm::CmpInst::ICMP_NE, testExpr, ConstantNull::get(
        test->location(), testExpr->type()));
  } else if (isa<AddressType>(testExpr->type())) {
    return new CompareExpr(test->location(), llvm::CmpInst::ICMP_NE, testExpr, ConstantNull::get(
        test->location(), testExpr->type()));
  } else if (const UnionType * ut = dyn_cast<UnionType>(testExpr->type())) {
    if (ut->isSingleNullableType()) {
      return new CompareExpr(test->location(), llvm::CmpInst::ICMP_NE, testExpr, ConstantNull::get(
          test->location(), testExpr->type()));
    }
  }

  // Cast to boolean.
  return BoolType::instance.implicitCast(test->location(), testExpr);
}

Defn * ExprAnalyzer::astToDefn(const ASTDecl * ast) {
  if (ast->nodeType() == ASTNode::VarList) {
    diag.error(ast) << "Multiple variable declarations not allowed here";
  }

  Defn * var = createLocalDefn(ast);
  var->setLocation(ast->location());
  return var;
}

bool ExprAnalyzer::astToDefnList(const ASTVarDecl * ast, DefnList & vars) {
  for (ASTDeclList::const_iterator it = ast->members().begin(); it != ast->members().end(); ++it) {
    Defn * var = createLocalDefn(*it);
    var->setLocation((*it)->location());
    vars.push_back(var);
  }

  return true;
}

LocalScope * ExprAnalyzer::createLocalScope(const char * scopeName) {
  DASSERT(activeScope() != NULL);
  LocalScope * newScope = new LocalScope(activeScope());
  newScope->setScopeName(scopeName);
  DASSERT(newScope->parentScope() != NULL);
  currentFunction_->localScopes().push_back(newScope);
  return newScope;
}

Defn * ExprAnalyzer::createLocalDefn(const ASTDecl * ast) {
  return ScopeBuilder::createLocalDefn(activeScope(), currentFunction_, ast);
}

Expr * ExprAnalyzer::createTempVar(Defn::DefnType kind, const char * name, Expr * value) {
  VariableDefn * var = new VariableDefn(kind, module(), name);
  var->addTrait(Defn::Singular);
  var->setLocation(value->location());
  var->setType(value->type());
  var->setStorageClass(Storage_Local);
  activeScope()->addMember(var);
  return new BinaryExpr(Expr::Prog2, value->location(), value->type(),
      new InitVarExpr(value->location(), var, value),
      LValueExpr::get(value->location(), NULL, var));
}

VariableDefn * ExprAnalyzer::createTempVar(
    const SourceLocation & loc, Defn::DefnType kind, const Type * type, const char * name) {
  VariableDefn * var = new VariableDefn(kind, module(), name);
  var->addTrait(Defn::Singular);
  var->setLocation(loc);
  var->setType(type);
  var->setStorageClass(Storage_Local);
  activeScope()->addMember(var);
  return var;
}

FunctionDefn * ExprAnalyzer::findInterfaceMethod(const CompositeType * type,
    const Type * interface, const char * method) {

  // Analyze the type.
  if (!AnalyzerBase::analyzeType(type, Task_PrepMemberLookup)) {
    return NULL;
  }

  // See if the type even has any members with that name.
  DefnList defns;
  if (!type->memberScope()->lookupMember(method, defns, true)) {
    return NULL;
  }

  // Analyze the interface.
  if (!AnalyzerBase::analyzeType(interface, Task_PrepMemberLookup)) {
    return NULL;
  }

  const CompositeType * interfaceClass = cast<CompositeType>(interface);
  if (!type->implements(interfaceClass)) {
    return NULL;
  }

  DefnList interfaceMethods;
  if (!type->memberScope()->lookupMember(method, interfaceMethods, true)) {
    diag.error(interface->typeDefn()) << "Interface '" << interface << "' does not define method '"
        << method << "'";
    return NULL;
  } else if (interfaceMethods.size() != 1) {
    diag.error(interface->typeDefn()) << "Interface '" << interface
        << "' has multiple definitions of method '" << method << "'";
  }

  FunctionDefn * interfaceMethod = cast<FunctionDefn>(interfaceMethods.front());
  for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
    if (FunctionDefn * fn = dyn_cast<FunctionDefn>(*it)) {
      if (fn->isOverrideOf(interfaceMethod)) {
        return fn;
      }
    }
  }

  return NULL;
}

const Type * ExprAnalyzer::setReturnType(const Type * returnType) {
  const Type * oldType = returnType_;
  returnType_ = returnType;
  return oldType;
}

LValueExpr * ExprAnalyzer::setMacroReturnVal(LValueExpr * retVal) {
  LValueExpr * oldVal = macroReturnVal_;
  macroReturnVal_ = retVal;
  return oldVal;
}

bool ExprAnalyzer::setInMacroExpansion(bool value) {
  bool prev = inMacroExpansion_;
  inMacroExpansion_ = value;
  return prev;
}

} // namespace tart
