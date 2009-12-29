/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionDefn.h"

#include "tart/Objects/Builtins.h"

#include "tart/Sema/ConstructorAnalyzer.h"

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
      if (var->type()->typeClass() != Type::Struct) {
        varIndices_[var] = varCount_++;
      }
    }
  }
}

void ConstructorAnalyzer::run(FunctionDefn * ctor) {
  // Check for blocks which end in return

  // If there's no superclass, and there's no variables to initialize, then there's
  // no need to check if anything is initialized.
  if (varIndices_.empty() && cls_->super() == NULL) {
    return;
  }

  BlockList returnBlocks;
  Block * entry = ctor->blocks().front();

  // For every block, determine which instance vars are initialized in that block.
  for (BlockList::const_iterator it = ctor->blocks().begin(); it != ctor->blocks().end(); ++it) {
    // See which vars are initialized within this block
    Block * blk = *it;
    BlockState & bstate = blockStates_[blk];
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
                if (selfParam == ctor->functionType()->selfParam()) {
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
    if (superCls->instanceFieldCountRecursive() > 0) {
      FunctionDefn * defaultSuperCtor = superCls->defaultConstructor();
      if (defaultSuperCtor == NULL) {
        diag.error(ctor) << "Missing call to superclass constructor";
        diag.info(ctor) << "(required because superclass does not have a default constructor.)";
      } else {
        // Synthesize a call to the superclass constructor.
        ParameterDefn * selfParam = ctor->functionType()->selfParam();
        DASSERT_OBJ(selfParam != NULL, ctor);
        DASSERT_OBJ(selfParam->type() != NULL, ctor);
        TypeDefn * selfType = selfParam->type()->typeDefn();
        DASSERT_OBJ(selfType != NULL, ctor);
        Expr * selfExpr = LValueExpr::get(selfParam->location(), NULL, selfParam);
        selfExpr = superCls->implicitCast(ctor->location(), selfExpr);

        defaultInitializers.push_back(
            new FnCallExpr(Expr::FnCall, ctor->location(), defaultSuperCtor, selfExpr));
      }
    }
  }

  // Determine if each instance var was initialized on all exit paths.
  const DefnList & fields = cls_->instanceFields();
  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it == NULL) {
      return;
    }

    VariableDefn * field = cast<VariableDefn>(*it);
    // TODO: Handle memberwise initialization of structs later.
    if (field->type()->typeClass() == Type::Struct) {
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
      }

      if (defaultValue == NULL) {
        diag.recovered();
        diag.error(ctor) << "Instance member '" << field->name() <<
            "' may not have been initialized.";
      } else {
        DFAIL("Implement");
      }
    }
  }

  entry->exprs().insert(entry->exprs().begin(),
      defaultInitializers.begin(), defaultInitializers.end());
}

} // namespace tart
