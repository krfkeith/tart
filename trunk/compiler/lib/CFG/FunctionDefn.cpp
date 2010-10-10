/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/Template.h"
#include "tart/Objects/Intrinsic.h"
#include "tart/Objects/Builtins.h"

#include <ostream>
#include <iostream>

namespace tart {

namespace {
  bool isOverloadable(Defn::DefnType dt) {
    return dt == Defn::Function || dt == Defn::Macro;
  }
}

// -------------------------------------------------------------------
// ParameterDefn

void ParameterDefn::trace() const {
  VariableDefn::trace();
  safeMark(internalType_);
}

void ParameterDefn::format(FormatStream & out) const {
  // Parameters are allowed to be unnamed (for function type declarations.)
  if (name_ != NULL) {
    out << name_;
  }

  if (out.getShowType() && type() != NULL) {
    out << ":" << type();
  }

  if (out.isVerbose() && initValue()) {
    out << "=" << initValue();
  }
}

// -------------------------------------------------------------------
// FunctionDefn
/** Constructor that takes an AST */
FunctionDefn::FunctionDefn(DefnType dtype, Module * m, const ASTFunctionDecl * ast)
  : ValueDefn(dtype, m, ast)
  , type_(NULL)
  , flags_(0)
  , dispatchIndex_(-1)
  , intrinsic_(NULL)
{
  if (modifiers_.flags & tart::Undef) {
    flags_ |= Undefined;
  }

  if (modifiers_.flags & tart::Override) {
    flags_ |= Override;
  }

  if (modifiers_.flags & tart::Abstract) {
    flags_ |= Abstract;
  }

  if (modifiers_.flags & tart::Final) {
    flags_ |= Final;
  }
}

/** Constructor that takes a name */
FunctionDefn::FunctionDefn(DefnType dtype, Module * m, const char * name)
  : ValueDefn(dtype, m, name)
  , type_(NULL)
  , flags_(0)
  , dispatchIndex_(-1)
  , intrinsic_(NULL)
{}

/** Constructor used for static type construction */
FunctionDefn::FunctionDefn(Module * m, const char * name, FunctionType * ty)
  : ValueDefn(Function, m, name)
  , type_(ty)
  , flags_(0)
  , dispatchIndex_(-1)
  , intrinsic_(NULL)
{}

const Type * FunctionDefn::type() const {
  return type_;
}

const Type * FunctionDefn::returnType() const {
  return type_->returnType();
}

bool FunctionDefn::hasBody() const {
  return (ast_ != NULL && functionDecl()->body() != NULL) || !blocks_.empty();
}

Expr * FunctionDefn::eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
  if (intrinsic_ != NULL) {
    return intrinsic_->eval(loc, this, self, args, NULL);
  }

  return NULL;
}

void FunctionDefn::trace() const {
  ValueDefn::trace();
  parameterScope_.trace();
  safeMark(type_);
  markList(blocks_.begin(), blocks_.end());
  markList(localScopes_.begin(), localScopes_.end());
}

bool FunctionDefn::isOverrideOf(const FunctionDefn * baseFunction) {
  if (this == baseFunction) {
    return true;
  }

  for (FunctionSet::iterator it = overriddenMethods_.begin(); it != overriddenMethods_.end();
      ++it) {
    if ((*it)->isOverrideOf(baseFunction)) {
      return true;
    }
  }

  return false;
}

bool FunctionDefn::hasSameSignature(const FunctionDefn * otherFn) const {
  const FunctionType * ft0 = functionType();
  const FunctionType * ft1 = otherFn->functionType();

  if (isErrorResult(ft0->returnType()) || isErrorResult(ft1->returnType())) {
    return false;
  }

  if (!ft0->returnType()->isEqual(ft1->returnType())) {
    return false;
  }

  if (ft0->params().size() != ft1->params().size()) {
    return false;
  }

  for (size_t i = 0; i < ft0->params().size(); ++i) {
    ParameterDefn * p0 = ft0->params()[i];
    ParameterDefn * p1 = ft1->params()[i];

    DASSERT_OBJ(p0->type() != NULL, p0);
    DASSERT_OBJ(p1->type() != NULL, p1);

    if (isErrorResult(p0->type()) || isErrorResult(p1->type())) {
      return false;
    }

    if (!p0->type()->isEqual(p1->type())) {
      return false;
    }

    if (p0->isVariadic() != p1->isVariadic()) {
      return false;
    }
  }

  return true;
}

bool FunctionDefn::canOverride(const FunctionDefn * base) const {
  DASSERT_OBJ(base->type() != NULL, base);
  DASSERT_OBJ(type() != NULL, this);

  const FunctionType * funcType = functionType();
  const FunctionType * baseType = base->functionType();

  const Type * baseReturn = baseType->returnType();
  const Type * funcReturn = funcType->returnType();

  if (!baseReturn->isEqual(funcReturn)) {
    // TODO: Variance test.
    return false;
  }

  size_t paramCount = baseType->params().size();
  if (paramCount != funcType->params().size()) {
    // Different number of parameters.
    return false;
  }

  for (size_t i = 0; i < paramCount; ++i) {
    const ParameterDefn * baseArg = baseType->params()[i];
    const ParameterDefn * funcArg = funcType->params()[i];

    if (baseArg->isVariadic() != funcArg->isVariadic()) {
      return false;
    }

    const Type * baseArgType = baseArg->type();
    const Type * funcArgType = funcArg->type();

    if (!baseArgType->isEqual(funcArgType)) {
      switch (baseArg->variance()) {
        case Invariant:
          // funcArgType must be equal to base type
          return false;

        case Covariant:
          // funcArgType is narrower than base type
          // TODO: 'isSubtype' is the wrong test here.
          //if (!funcArgType->isSubtype(baseArgType)) {
          //  return false;
          //}
          return false;
          break;

        case Contravariant:
          // funcArgType is broader than base type
          // TODO: 'isSubtype' is the wrong test here.
          //if (!baseArgType->isSubtype(funcArgType)) {
          //  return false;
          //}
          return false;
          break;
      }
    }
  }

  return true;
}

const std::string & FunctionDefn::linkageName() const {
  if (lnkName.empty()) {
    Defn::linkageName();

    if (!isExtern()) {
      if (!params().empty()) {
        lnkName.append("(");
        for (ParameterList::const_iterator it = params().begin(); it != params().end(); ++it) {
          if (it != params().begin()) {
            lnkName.append(",");
          }
          typeLinkageName(lnkName, (*it)->type());
          if ((*it)->getFlag(ParameterDefn::Variadic)) {
            lnkName.append("...");
          }
        }
        lnkName.append(")");
      }

      if (!type_->returnType()->isVoidType()) {
        lnkName.append("->");
        typeLinkageName(lnkName, type_->returnType());
      }
    }
  }

  return lnkName;
}

void FunctionDefn::format(FormatStream & out) const {
  if (out.isVerbose()) {
    out << "def ";
  }

  if (out.getShowQualifiedName() && !qname_.empty()) {
    if (out.getShowType() && parentDefn()) {
      out << parentDefn() << "." << name_;
    } else {
      out << qname_;
    }
  } else {
    out << name_;
  }

  if (out.getShowType()) {
    const TemplateSignature * ts = templateSignature();
    if (ts != NULL) {
      ts->format(out);
    }

    if (type_ != NULL) {
      out << "(";
      formatParameterList(out, params());
      out << ")";
      if (type_->returnType() != NULL && !type_->returnType()->isVoidType()) {
        out << " -> " << type_->returnType();
      }
    }
  }
}

void FunctionDefn::dumpBlocks() {
  FormatStream stream(std::cout);

  // Number all blocks.
  int index = 0;
  for (BlockList::iterator bi = blocks_.begin(); bi != blocks_.end(); ++bi) {
    (*bi)->setIndex(++index);
  }

  // Print out blocks.
  stream << "def " << this << ":\n";
  for (BlockList::iterator bi = blocks_.begin(); bi != blocks_.end(); ++bi) {
    Block * blk = *bi;
    if (blk->unwindTarget()) {
      stream << "  " << blk << " (unwind " << blk->unwindTarget() << "):\n";
    } else {
      stream << "  " << blk << ":\n";
    }
    for (ExprList::iterator ei = blk->exprs().begin(); ei != blk->exprs().end(); ++ei) {
      stream << "    " << *ei << "\n";
    }

    switch (blk->terminator()) {
      case BlockTerm_None:
        stream << "    **no terminator**\n";
        break;

      case BlockTerm_Branch:
        stream << "    br " << blk->succs().front() << "\n";
        break;

      case BlockTerm_Conditional:
        stream << "    if (" << blk->termValue() << ") br " << blk->succs()[0] <<
            " else br " << blk->succs()[1] << "\n";
        break;

      case BlockTerm_Return:
        if (blk->termValue() != NULL) {
          stream << "    return " << blk->termValue() << "\n";
        } else {
          stream << "    return\n";
        }
        break;

      case BlockTerm_Switch: {
        break;
      }

      case BlockTerm_Throw:
        if (blk->termValue() != NULL) {
          stream << "    throw " << blk->termValue() << "\n";
        } else {
          stream << "    throw\n";
        }
        break;

      case BlockTerm_ResumeUnwind:
        stream << "    resume unwind\n";
        break;

      case BlockTerm_LocalReturn:
        stream << "    local return\n";
        break;

      case BlockTerm_Catch:
      case BlockTerm_TraceCatch: {
        size_t numSelectors = blk->termExprs().size() - 1;
        for (size_t i = 0; i < numSelectors; ++i) {
          Expr * catchExpr = blk->termExprs()[i + 1];
          Block * catchBody = blk->succs()[i];
          if (catchExpr) {
            stream << "    catch " << catchExpr << ": " << catchBody << "\n";
          } else {
            stream << "    catch *: " << catchBody << "\n";
          }
        }

        break;
      }
    }
  }
  stream << "\n";
}

} // namespace tart
