/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Type.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

#ifdef TYPE_CLASS
#undef TYPE_CLASS
#endif

#define TYPE_CLASS(x) #x,

const char * TypeClassNames[] = {
#include "tart/CFG/TypeClass.def"
};

#undef TYPE_CLASS

const char * compatibilityError(ConversionRank rank) {
  switch (rank) {
    case Incompatible:
      return "Type mismatch";
      break;

    case Truncation:
      return "Truncation of value";
      break;

    case IntegerToBool:
      return "Implicit conversion of integer to bool type";
      break;

    case PrecisionLoss:
      return "Possible loss of precision";
      break;

    case SignedUnsigned:
      return "Signed/unsigned mismatch";
      break;

    default:
      DFAIL("IllegalState");
  }
}

void compatibilityWarning(const SourceLocation & loc,
  ConversionRank rank, const Type * from, const Type * to) {
  DASSERT(!isErrorResult(from));
  if (isConversionWarning(rank)) {
    diag.error(loc) << Format_Verbose << compatibilityError(rank) <<
        " converting from " << from << " to " << to;
  }
}

FormatStream & operator<<(FormatStream & out, ConversionRank rank) {
  switch (rank) {
    case Incompatible: {
      out << "Incompatible";
      break;
    }

    case Truncation: {
      out << "Truncation";
      break;
    }

    case IntegerToBool: {
      out << "IntegerToBool";
      break;
    }

    case SignedUnsigned: {
      out << "SignedUnsigned";
      break;
    }

    case PrecisionLoss: {
      out << "PrecisionLoss";
      break;
    }

    case NonPreferred: {
      out << "NonPreferred";
      break;
    }

    case ExactConversion: {
      out << "ExactConversion";
      break;
    }

    case IdenticalTypes: {
      out << "IdenticalTypes";
      break;
    }
  }

  return out;
}

// -------------------------------------------------------------------
// Represents a type conversion operation.
Conversion::Conversion(const Type * from)
  : fromType(from)
  , fromValue(NULL)
  , resultValue(NULL)
  , bindingEnv(NULL)
{}

Conversion::Conversion(Expr * from)
  : fromType(from->type())
  , fromValue(from)
  , resultValue(NULL)
  , bindingEnv(NULL)
{}

Conversion::Conversion(Expr * from, Expr ** to)
  : fromType(from->type())
  , fromValue(from)
  , resultValue(to)
  , bindingEnv(NULL)
{}

const Type * Conversion::getFromType() const {
  const Type * ty = dealias(fromType);

  if (bindingEnv != NULL) {
    while (const PatternVar * pvar = dyn_cast_or_null<PatternVar>(ty)) {
      ty = dealias(bindingEnv->get(pvar));
    }
  }

  return ty;
}

// -------------------------------------------------------------------
// Type
void Type::trace() const {}

const char * Type::typeClassName(TypeClass tc) {
  uint32_t index = (uint32_t)tc;
  if (index < KindCount) {
    return TypeClassNames[index];
  }
  return "<Invalid Type>";
}

bool Type::isEqual(const Type * other) const {
  return this == dealias(other);
}

bool Type::isVoidType() const {
  return cls == Primitive && static_cast<const PrimitiveType *>(this)->typeId() == TypeId_Void;
}

bool Type::isUnsizedIntType() const {
  return cls == Primitive &&
      static_cast<const PrimitiveType *>(this)->typeId() == TypeId_UnsizedInt;
}

ConversionRank Type::convert(const Conversion & cn) const {
  if (cn.resultValue == NULL) {
    // Ask the constraint if we can convert to this type. Most types don't
    // know about constraints, but constraints know about most types.
    ConversionRank rank = cn.fromType->convertTo(this);
    if (rank != Incompatible) {
      return rank;
    }
  }

  return convertImpl(cn);
}

ConversionRank Type::canConvert(Expr * fromExpr) const {
  return convert(Conversion(fromExpr));
}

ConversionRank Type::canConvert(const Type * fromType) const {
  return convert(Conversion(fromType));
}

Expr * Type::implicitCast(const SourceLocation & loc, Expr * from) const {
  Expr * result = NULL;
  ConversionRank tc = convert(Conversion(from, &result));
  compatibilityWarning(loc, tc, from->type(), this);
  DASSERT(tc == Incompatible || result != NULL);
  return result;
}

Expr * Type::explicitCast(const SourceLocation & loc, Expr * from) const {
  Expr * result = NULL;
  ConversionRank tc = convert(Conversion(from, &result));
  if (tc == Incompatible) {
    compatibilityWarning(loc, tc, from->type(), this);
  }
  return result;
}

// -------------------------------------------------------------------
// DeclaredType

DeclaredType::DeclaredType(TypeClass cls, TypeDefn * de, Scope * parentScope)
  : TypeImpl(cls)
  , IterableScope(parentScope)
  , defn_(de)
{
  DASSERT(de != NULL);
  setScopeName(de->name());
}

  /** Return the number of type parameters of this type. */
size_t DeclaredType::numTypeParams() const {
  TemplateSignature * tsig = defn_->templateSignature();
  if (tsig != NULL) {
    return tsig->patternVarCount();
  }

  TemplateInstance * tinst = defn_->templateInstance();
  if (tinst != NULL) {
    return tinst->paramValues().size();
  }

  return 0;
}

Type * DeclaredType::typeParam(int index) const {
  TemplateSignature * tsig = defn_->templateSignature();
  if (tsig != NULL) {
    return tsig->patternVar(index);
  }

  TemplateInstance * tinst = defn_->templateInstance();
  if (tinst != NULL) {
    return tinst->paramValues()[index];
  }

  DFAIL("Illegal State");
}

void DeclaredType::trace() const {
  safeMark(defn_);
  IterableScope::trace();
}

void DeclaredType::format(FormatStream & out) const {
  defn_->format(out);
}

// -------------------------------------------------------------------
// TypeAlias

TypeAlias::TypeAlias(Type * val)
  : Type(Alias)
  , value_(val)
{
}

const llvm::Type * TypeAlias::irType() const {
  DASSERT(value_ != NULL);
  return value_->irType();
}

const llvm::Type * TypeAlias::irEmbeddedType() const {
  return value_->irEmbeddedType();
}

const llvm::Type * TypeAlias::irParameterType() const {
  return value_->irParameterType();
}

ConversionRank TypeAlias::convertImpl(const Conversion & conversion) const {
  DASSERT(value_ != NULL);
  return value_->convertImpl(conversion);
}

void TypeAlias::format(FormatStream & out) const {
  DASSERT(value_ != NULL);
  return value_->format(out);
}

void TypeAlias::trace() const {
  safeMark(value_);
}

// -------------------------------------------------------------------
// NonTypeConstant

NonTypeConstant * NonTypeConstant::get(ConstantExpr * value) {
  // TODO: Fold unique values.
  return new NonTypeConstant(value);
}

bool NonTypeConstant::isEqual(const Type * other) const {
  if (const NonTypeConstant * ntc = dyn_cast<NonTypeConstant>(other)) {
    return value_->isEqual(ntc->value());
  }

  return false;
}

const llvm::Type * NonTypeConstant::irType() const {
  DFAIL("IllegalState");
}

ConversionRank NonTypeConstant::convertImpl(const Conversion & conversion) const {
  DFAIL("IllegalState");
}

Expr * NonTypeConstant::nullInitValue() const {
  DFAIL("IllegalState");
}

void NonTypeConstant::trace() const {
  value_->mark();
}

void NonTypeConstant::format(FormatStream & out) const {
  out << value_;
}

// -------------------------------------------------------------------
// Utility functions

Type * Type::selectMoreSpecificType(Type * type1, Type * type2) {
  if (type2->includes(type1)) {
    return type1;
  } else if (type1->includes(type2)) {
    return type2;
  } else {
    diag.debug() << "Neither " << type1 << " nor " << type2 << " is more specific than the other.";
    return NULL;
  }
}

Type * Type::selectLessSpecificType(Type * type1, Type * type2) {
  if (type2->includes(type1)) {
    return type2;
  } else if (type1->includes(type2)) {
    return type1;
  } else {
    const Type * t1 = PrimitiveType::derefEnumType(type1);
    const Type * t2 = PrimitiveType::derefEnumType(type2);
    if (t1->typeClass() == Type::Primitive && t2->typeClass() == Type::Primitive) {
      const PrimitiveType * p1 = static_cast<const PrimitiveType *>(t1);
      const PrimitiveType * p2 = static_cast<const PrimitiveType *>(t2);

      if (isIntegerType(p1->typeId()) && isIntegerType(p2->typeId())) {
        bool isSignedResult = isSignedIntegerType(p1->typeId())
            || isSignedIntegerType(p2->typeId());
        int type1Bits = p1->numBits() + (isSignedResult && isUnsignedIntegerType(p1->typeId()) ? 1 : 0);
        int type2Bits = p2->numBits() + (isSignedResult && isUnsignedIntegerType(p2->typeId()) ? 1 : 0);
        int resultBits = std::max(type1Bits, type2Bits);

        if (isSignedResult) {
          if (resultBits <= 8) {
            return &ByteType::instance;
          } else if (resultBits <= 16) {
            return &ShortType::instance;
          } else if (resultBits <= 32) {
            return &IntType::instance;
          } else if (resultBits <= 64) {
            return &LongType::instance;
          }

          diag.error() << "Integer value requires " << resultBits << " bits, too large.";
          diag.info() << "p1 = " << p1;
          diag.info() << "p2 = " << p2;
          //DFAIL("Integer value too large to be represented as native type.");
        } else {
          if (resultBits <= 8) {
            return &UByteType::instance;
          } else if (resultBits <= 16) {
            return &UShortType::instance;
          } else if (resultBits <= 32) {
            return &UIntType::instance;
          } else if (resultBits <= 64) {
            return &ULongType::instance;
          }

          diag.error() << "Integer value requires " << resultBits << " bits, too large.";
          diag.info() << "p1 = " << p1;
          diag.info() << "p2 = " << p2;
          //DFAIL("Integer value too large to be represented as native type.");
        }
      }
    }

    diag.debug() << "Neither " << type1 << " nor " << type2 << " is more specific than the other.";
    return NULL;
  }
}

Type * findCommonType(Type * t0, Type * t1) {
  DASSERT(t0 != NULL);
  DASSERT(t1 != NULL);

  if (t0->isEqual(t1)) {
    return t0;
  }

  ConversionRank tc0 = t0->canConvert(t1);
  ConversionRank tc1 = t1->canConvert(t0);
  if (tc1 > tc0) {
    return t1;
  } else if (tc0 > tc1) {
    return t0;
  } else {
    return NULL;
  }
}

Type * dealiasImpl(Type * t) {
  while (t != NULL && t->typeClass() == Type::Alias) {
    if (TypeAlias * alias = dyn_cast<TypeAlias>(t)) {
      t = alias->value();
      DASSERT_OBJ(t != NULL, alias);
    } else {
      break;
    }
  }

  while (const PatternValue * pval = dyn_cast_or_null<PatternValue>(t)) {
    Type * v = pval->value();
    if (v != NULL) {
      t = v;
    } else {
      break;
    }
  }

  return t;
}

const Type * dealias(const Type * t) {
  return dealiasImpl(const_cast<Type *>(t));
}

Type * dealias(Type * t) {
  return dealiasImpl(t);
}

} // namespace tart
