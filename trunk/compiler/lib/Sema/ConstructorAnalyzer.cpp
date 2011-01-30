/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/FunctionDefn.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/ConstructorAnalyzer.h"
#include "tart/Sema/AnalyzerBase.h"

namespace tart {

ConstructorAnalyzer::ConstructorAnalyzer(CompositeType * cls)
  : cls_(cls)
  , varCount_(1) // 0 is for super() call.
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

  // Check for blocks which end in return
  BlockList returnBlocks;
  Block * entry = ctor->blocks().front();
  FunctionType * ctorType = ctor->functionType();

  DASSERT(!ctor->blocks().empty());

  // For every block, determine which instance vars are initialized in that block.
  for (BlockList::const_iterator it = ctor->blocks().begin(); it != ctor->blocks().end(); ++it) {
    // See which vars are initialized within this block
    Block * blk = *it;
    BlockState & bstate = blockStates_[blk];
    bstate.visited_ = false;
    bstate.initialized_.resize(varCount_, false);

    for (ExprList::const_iterator it = blk->exprs().begin(); it != blk->exprs().end(); ++it) {
      Expr * e = *it;
      switch (e->exprType()) {
        case Expr::Assign:
        case Expr::PostAssign: {
          do {
            AssignmentExpr * assignExpr = static_cast<AssignmentExpr *>(e);
            if (LValueExpr * lval = dyn_cast<LValueExpr>(assignExpr->toExpr())) {
              if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
                VarIndexMap::iterator entry = varIndices_.find(var);
                if (entry != varIndices_.end()) {
                  bstate.initialized_.set(entry->second);
                }
              }
            }

            e = assignExpr->fromExpr();
          } while (e->exprType() == Expr::Assign);
          break;
        }

        case Expr::FnCall: {
          FnCallExpr * callExpr = static_cast<FnCallExpr *>(e);
          FunctionDefn * fn = callExpr->function();
          if (fn->isCtor()) {
            // We need to know if we're calling ourselves, or if we're calling super().
            // If it's ourselves, then we can assume that this ctor inits everything.
            Expr * selfExpr = callExpr->selfArg();
            bool isSuperCall = false;
            if (selfExpr->exprType() == Expr::UpCast) {
              CastExpr * castExpr = static_cast<CastExpr *>(selfExpr);
              selfExpr = castExpr->arg();
              isSuperCall = true;
            }

            if (LValueExpr * selfLVal = dyn_cast<LValueExpr>(selfExpr)) {
              if (ParameterDefn * selfParam = dyn_cast<ParameterDefn>(selfLVal->value())) {
                if (selfParam == ctorType->selfParam()) {
                  if (isSuperCall) {
                    bstate.initialized_.set(0);
                  } else {
                    // If it's a call to another constructor of this class, then
                    // assume that everything was initialized by the other constructor.
                    // Note that you can fool this code with cyclic constructor calls.
                    // TODO: Check for cyclic constructor calls.
                    return;
                  }
                }
              }
            }
          }

          break;
        }

        default:
          break;
      }
    }

    if (blk->terminator() == BlockTerm_Return) {
      returnBlocks.push_back(blk);
    }
  }

  // A simple (yet incorrect) flow analysis...
  // TODO: Do this with real dominance graph analysis.
  for (BlockList::const_iterator it = returnBlocks.begin(); it != returnBlocks.end(); ++it) {
    visitBlock(*it);
  }

  // List of additional initializers to add.
  ExprList defaultInitializers;

  // Determine if the superclass constructor was called on all exit paths.
  bool needsSuperCall = false;
  for (BlockList::const_iterator it = returnBlocks.begin(); it != returnBlocks.end(); ++it) {
    Block * blk = *it;
    BlockState & bstate = blockStates_[blk];

    if (!bstate.initialized_[0] && cls_->super() != NULL && cls_->super() != Builtins::typeObject) {
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
      DASSERT_OBJ(selfParam->type() != NULL, ctor);
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
    for (BlockList::const_iterator it = returnBlocks.begin(); it != returnBlocks.end(); ++it) {
      Block * blk = *it;
      BlockState & bstate = blockStates_[blk];
      if (!bstate.initialized_[index]) {
        needsInitialization = true;
      }
    }

    if (needsInitialization) {
      Expr * defaultValue = field->initValue();
      const Type * fieldType = field->type();
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
  entry->exprs().insert(entry->exprs().begin(),
      defaultInitializers.begin(), defaultInitializers.end());
}

void ConstructorAnalyzer::visitBlock(Block * b) {
  BlockState & bstate = blockStates_[b];
  if (bstate.visited_) {
    return;
  }

  // Get the intersection of all predecessor blocks.
  bstate.visited_ = true;
  if (!b->preds().empty()) {
    BlockState combinedStates;
    combinedStates.initialized_.resize(varCount_, true);
    for (BlockList::const_iterator it = b->preds().begin(); it != b->preds().end(); ++it) {
      Block * pred = *it;
      visitBlock(pred);
      combinedStates.initialized_ &= blockStates_[pred].initialized_;
    }

    // Now union it with the current block.
    bstate.initialized_ |= combinedStates.initialized_;
  }
}

} // namespace tart
