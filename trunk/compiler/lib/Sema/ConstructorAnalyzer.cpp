/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Defn/FunctionDefn.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/ConstructorAnalyzer.h"
#include "tart/Sema/AnalyzerBase.h"

namespace tart {

ConstructorAnalyzer::ConstructorAnalyzer(CompositeType * cls)
  : cls_(cls)
  , varCount_(1) // 0 is for super() call.
  , isReturnVisited_(false)
{
  // Collect a set of all initializable members and assign an index to each one.
  const DefnList & fields = cls->instanceFields();
  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it != NULL) {
      VariableDefn * var = cast<VariableDefn>(*it);
      // TODO: Handle memberwise initialization of structs later.
      if (var->type()->typeClass() != Type::Struct &&
          var->type()->typeClass() != Type::NArray &&
          var->type()->typeClass() != Type::FlexibleArray) {
        varIndices_[var] = varCount_++;
      }
    }
  }
}

void ConstructorAnalyzer::run(FunctionDefn * ctor) {
  // If there's no superclass, and there's no variables to initialize, then there's
  // no need to check if anything is initialized.
  if (varIndices_.empty() && cls_->super() == NULL) {
    return;
  }

  // Don't do classes whose instance vars are filled in by the compiler.
  if (cls_ == Builtins::typeObject ||
      cls_ == Builtins::typeTypeInfoBlock ||
      cls_ == Builtins::typeThrowable) {
    return;
  }

  bool trace = AnalyzerBase::isTraceEnabled(ctor->parentDefn());
  if (trace) {
    diag.debug(ctor) << Format_Verbose << "Analyzing constructor " << ctor;
  }

  set_.resize(varCount_);
  maybeSet_.resize(varCount_);
  ctor_ = ctor;

  DASSERT(ctor->body() != NULL);

  CtorInitState initState(*this);
  initState.visit(ctor);
  initState.visitReturn(NULL);

  FunctionType * ctorType = ctor->functionType();

  // List of additional initializers to add.
  ExprList defaultInitializers;

  // Determine if the superclass constructor was called on all exit paths.
  bool needsSuperCall = false;
  if (cls_->super() != NULL && cls_->super() != Builtins::typeObject) {
    if (!set_[0]) {
      needsSuperCall = true;
    }
  }

  if (needsSuperCall) {
    CompositeType * superCls = cls_->super();
    // See if the superclass has any fields at all.
    FunctionDefn * defaultSuperCtor = superCls->defaultConstructor();
    if (defaultSuperCtor == NULL) {
      diag.error(ctor) << "Missing call to superclass constructor";
      diag.info(ctor) << "(required because superclass does not have a default constructor.)";
    } else {
      FunctionType * superCtorType = defaultSuperCtor->functionType();
      if (trace) {
        diag.debug(ctor) << Format_Verbose << "Adding call to super() in " << ctor;
      }
      // Synthesize a call to the superclass constructor.
      ParameterDefn * selfParam = ctorType->selfParam();
      DASSERT_OBJ(selfParam != NULL, ctor);
      DASSERT_OBJ(selfParam->type(), ctor);
      TypeDefn * selfType = selfParam->type()->typeDefn();
      DASSERT_OBJ(selfType != NULL, ctor);
      Expr * selfExpr = LValueExpr::get(selfParam->location(), NULL, selfParam);
      selfExpr = superCls->implicitCast(ctor->location(), selfExpr);
      FnCallExpr * superCall = new FnCallExpr(
          Expr::FnCall, ctor->location(), defaultSuperCtor, selfExpr);

      // Fill in default params
      size_t paramCount = superCtorType->params().size();
      for (size_t paramIndex = 0; paramIndex < paramCount; ++paramIndex) {
        ParameterDefn * param = superCtorType->params()[paramIndex];
        if (param->initValue() == NULL) {
          diag.error(ctor) << "Attempting to call a default superclass constructor, but parameter "
              << (paramIndex + 1) << " does not have a default value.";
          diag.info(param) << param;
          break;
        } else if (param->isVariadic()) {
          // Pass a null array - possibly a static singleton.
          ArrayLiteralExpr * arrayParam =
              AnalyzerBase::createArrayLiteral(param->location(), param->type());
          AnalyzerBase::analyzeType(arrayParam->type(), Task_PrepMemberLookup);
          superCall->appendArg(arrayParam);
        } else {
          superCall->appendArg(param->initValue());
        }
      }

      defaultInitializers.push_back(superCall);
    }
  }

  // Determine if each instance var was initialized on all exit paths.
  const DefnList & fields = cls_->instanceFields();
  Expr * selfArg = NULL;
  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it == NULL) {
      continue;
    }

    VariableDefn * field = cast<VariableDefn>(*it);
    // TODO: Handle memberwise initialization of structs later.
    if (field->type()->typeClass() == Type::Struct ||
        field->type()->typeClass() == Type::NArray ||
        field->type()->typeClass() == Type::FlexibleArray) {
      continue;
    }

    int index = varIndices_[field];
    bool needsInitialization = false;
    if (!set_[index]) {
      needsInitialization = true;
    }

    if (needsInitialization) {
      Expr * defaultValue = field->initValue();
      const Type * fieldType = field->type().unqualified();
      if (defaultValue == NULL) {
        if (field->defnType() == Defn::Let) {
          diag.error(field) << "Instance member '" << field->name() <<
              "' defined with 'let' should be explicitly initialized.";
        }

        defaultValue = fieldType->nullInitValue();
        if (defaultValue == NULL) {
          if (fieldType->isReferenceType()) {
            defaultValue = ConstantNull::get(field->location(), fieldType);
          } else if (const UnionType * utype = dyn_cast<UnionType>(fieldType)) {
            if (utype->isSingleOptionalType()) {
              if (utype->hasRefTypesOnly()) {
                defaultValue = utype->implicitCast(field->location(),
                    ConstantNull::get(field->location(), fieldType));
              } else {
                DFAIL("Implement default construction of non-ref unions");
              }
            }
          }
        }
      }

      if (defaultValue == NULL) {
        diag.recovered();
        diag.error(ctor) << "Instance member '" << field->name() <<
            "' may not have been initialized.";
      } else {
        if (selfArg == NULL) {
          selfArg = LValueExpr::get(ctor->location(), NULL, ctor->functionType()->selfParam());
        }
        Expr * assign = new AssignmentExpr(Expr::Assign, field->location(),
            LValueExpr::get(field->location(), selfArg, field), defaultValue);
        defaultInitializers.push_back(assign);
      }
    }
  }

  // TODO: There's also an ordering issue, as some fields may depend on others.
  SeqExpr * bodyExpr = cast<SeqExpr>(ctor->body());
  bodyExpr->args().insert(bodyExpr->begin(),
      defaultInitializers.begin(), defaultInitializers.end());
}

void ConstructorAnalyzer::putReturnState(llvm::BitVector & set, llvm::BitVector & maybeSet) {
  if (isReturnVisited_) {
    set_ &= set;
    maybeSet_ |= maybeSet;
  } else {
    set_ = set;
    maybeSet_ = maybeSet;
    isReturnVisited_ = true;
  }
}

// -------------------------------------------------------------------
// CtorInitState

CtorInitState::CtorInitState(ConstructorAnalyzer & analyzer)
  : analyzer_(analyzer)
{
  set_.resize(analyzer_.set_.size());
  maybeSet_.resize(analyzer_.maybeSet_.size());
}

void CtorInitState::checkAssign(AssignmentExpr * in) {
  if (LValueExpr * lval = dyn_cast<LValueExpr>(in->toExpr())) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
      VarIndexMap::iterator entry = analyzer_.varIndices_.find(var);
      if (entry != analyzer_.varIndices_.end()) {
        set_.set(entry->second);
        maybeSet_.set(entry->second);
      }
    }
  }
}

Expr * CtorInitState::visitAssign(AssignmentExpr * in) {
  CFGPass::visitAssign(in);
  checkAssign(in);
  return in;
}

Expr * CtorInitState::visitPostAssign(AssignmentExpr * in) {
  CFGPass::visitPostAssign(in);
  checkAssign(in);
  return in;
}

Expr * CtorInitState::visitFnCall(FnCallExpr * in) {
  CFGPass::visitFnCall(in);
  FunctionDefn * fn = in->function();
  if (fn->isCtor()) {
    // We need to know if we're calling ourselves, or if we're calling super().
    // If it's ourselves, then we can assume that this ctor inits everything.
    Expr * selfExpr = in->selfArg();
    bool isSuperCall = false;
    if (selfExpr->exprType() == Expr::UpCast) {
      CastExpr * castExpr = static_cast<CastExpr *>(selfExpr);
      selfExpr = castExpr->arg();
      isSuperCall = true;
    }

    if (LValueExpr * selfLVal = dyn_cast<LValueExpr>(selfExpr)) {
      if (ParameterDefn * selfParam = dyn_cast<ParameterDefn>(selfLVal->value())) {
        if (selfParam == analyzer_.ctor_->functionType()->selfParam()) {
          if (isSuperCall) {
            set_.set(0);
          } else {
            // If it's a call to another constructor of this class, then
            // assume that everything was initialized by the other constructor.
            // Note that you can fool this code with cyclic constructor calls.
            // TODO: Check for cyclic constructor calls.
            set_.reset();
            set_.flip();
            maybeSet_ = set_;
          }
        }
      }
    }
  }

  return in;
}

Expr * CtorInitState::visitIf(IfExpr * in) {
  visitExpr(in->test());
  CtorInitState thenState(analyzer_);
  thenState.visitExpr(in->thenVal());
  maybeSet_ |= thenState.maybeSet_;
  CtorInitState elseState(analyzer_);
  elseState.visitExpr(in->elseVal());
  set_ |= (thenState.set_ & elseState.set_);
  maybeSet_ |= elseState.maybeSet_;
  return in;
}

Expr * CtorInitState::visitWhile(WhileExpr * in) {
  visitExpr(in->test());
  CtorInitState loopState(analyzer_);
  loopState.visitExpr(in->body());
  maybeSet_ |= loopState.maybeSet_;
  return in;
}

Expr * CtorInitState::visitDoWhile(WhileExpr * in) {
  visitExpr(in->test());
  CtorInitState loopState(analyzer_);
  loopState.visitExpr(in->body());
  maybeSet_ |= loopState.maybeSet_;
  return in;
}

Expr * CtorInitState::visitFor(ForExpr * in) {
  visitExpr(in->init());
  visitExpr(in->test());
  CtorInitState loopState(analyzer_);
  loopState.visitExpr(in->incr());
  loopState.visitExpr(in->body());
  maybeSet_ |= loopState.maybeSet_;
  return in;
}

Expr * CtorInitState::visitForEach(ForEachExpr * in) {
  visitExpr(in->iterator());
  visitExpr(in->test());
  CtorInitState loopState(analyzer_);
  loopState.visitExpr(in->body());
  maybeSet_ |= loopState.maybeSet_;
  return in;
}

Expr * CtorInitState::visitSwitch(SwitchExpr * in) {
  // We only care about switch statements that have an 'else' case.
  if (in->elseCase() != NULL) {
    CtorInitState switchState(analyzer_);
    switchState.visitExpr(in->elseCase());
    for (SwitchExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
      CtorInitState caseState(analyzer_);
      caseState.visitExpr(*it);
      switchState.set_ &= caseState.set_;
      switchState.maybeSet_ |= caseState.maybeSet_;
    }

    maybeSet_ |= switchState.maybeSet_;
    set_ |= switchState.set_;
  }
  return in;
}

Expr * CtorInitState::visitMatch(MatchExpr * in) {
  // We only care about match statements that have an 'else' case.
  if (in->elseCase() != NULL) {
    CtorInitState matchState(analyzer_);
    matchState.visitExpr(in->elseCase());
    for (SwitchExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
      CtorInitState caseState(analyzer_);
      caseState.visitExpr(*it);
      matchState.set_ &= caseState.set_;
      matchState.maybeSet_ |= caseState.maybeSet_;
    }

    maybeSet_ |= matchState.maybeSet_;
    set_ |= matchState.set_;
  }
  return in;
}

Expr * CtorInitState::visitTry(TryExpr * in) {
  // TODO: Implement
  DFAIL("Implement");
  return in;
}

Expr * CtorInitState::visitReturn(ReturnExpr * in) {
  analyzer_.putReturnState(set_, maybeSet_);
  return in;
}

} // namespace tart
