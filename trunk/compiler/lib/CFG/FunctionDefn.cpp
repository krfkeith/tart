/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/Block.h"
#include "tart/Objects/Intrinsic.h"
#include "tart/Objects/Builtins.h"

#include <ostream>
#include <iostream>

namespace tart {

namespace {
  bool isOverloadable(Defn::DefnType dt) {
    return dt == Defn::Function || dt == Defn::Macro;
  }

  // Given a type, append the linkage name of that type to the output buffer.
  void typeLinkageName(std::string & out, Type * ty) {
    ty = dealias(ty);
    if (TypeDefn * td = ty->typeDefn()) {
      out.append(td->linkageName());
    } else if (FunctionType * ftype = dyn_cast<FunctionType>(ty)) {
      out.append("fn");
      if (!ftype->params().empty()) {
        out.append("(");
        ParameterList & params = ftype->params();
        for (ParameterList::iterator it = params.begin(); it != params.end(); ++it) {
          if (it != params.begin()) {
            out.append(",");
          }

          typeLinkageName(out, (*it)->type());
        }
        out.append(")");
      }

      if (ftype->returnType() != NULL && !ftype->returnType()->isVoidType()) {
        out.append("->");
        typeLinkageName(out, ftype->returnType());
      }
    } else if (UnionType * utype = dyn_cast<UnionType>(ty)) {
      for (TypeList::iterator it = utype->members().begin(); it != utype->members().end(); ++it) {
        if (it != utype->members().begin()) {
          out.append("|");
        }

        typeLinkageName(out, *it);
      }
    } else {
      diag.error() << "Type: " << ty;
      DFAIL("Can't compute linkage name of type");
    }
  }
}

// -------------------------------------------------------------------
// ParameterDefn

void ParameterDefn::trace() const {
  ValueDefn::trace();
  safeMark(type_);
  safeMark(internalType_);
  safeMark(defaultValue_);
}

void ParameterDefn::format(FormatStream & out) const {
  // Parameters are allowed to be unnamed (for function type declarations.)
  if (name_ != NULL) {
    if (out.getShowQualifiedName()) {
      out << qname_;
    } else {
      out << name_;
    }
  }

  if (out.getShowType() && type_) {
    out << ":" << type_;
  }

  if (out.isVerbose() && defaultValue_) {
    out << "=" << defaultValue_;
  }
}

// -------------------------------------------------------------------
// FunctionDefn
Type * FunctionDefn::type() const {
  return type_;
}

Type * FunctionDefn::returnType() const {
  return type_->returnType();
}

bool FunctionDefn::hasBody() const {
  return (ast_ != NULL && functionDecl()->body() != NULL) || !blocks_.empty();
}

Expr * FunctionDefn::eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
  if (intrinsic_ != NULL) {
    return intrinsic_->eval(loc, self, args, NULL);
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

const std::string & FunctionDefn::linkageName() const {
  if (lnkName.empty()) {
    Defn::linkageName();

    if (!isExtern()) {
      if (!type_->params().empty()) {
        lnkName.append("(");
        for (ParameterList::iterator it = type_->params().begin(); it != type_->params().end();
            ++it) {
          if (it != type_->params().begin()) {
            lnkName.append(",");
          }
          typeLinkageName(lnkName, (*it)->type());
          if ((*it)->getFlag(ParameterDefn::Variadic)) {
            lnkName.append("...");
          }
        }
        lnkName.append(")");
      }

      if (type_->returnType() != NULL && !type_->returnType()->isVoidType()) {
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
    out << qname_;
  } else {
    out << name_;
  }

  if (out.getShowType() && type_ != NULL) {
    out << "(";
    formatParameterList(out, type_->params());
    out << ")";
    if (type_->returnType() != NULL && !type_->returnType()->isVoidType()) {
      out << " -> " << type_->returnType();
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

      case BlockTerm_Catch: {
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
