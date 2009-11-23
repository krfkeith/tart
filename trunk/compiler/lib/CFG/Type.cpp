/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Type.h"
#include "tart/CFG/TypeAlias.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
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

void compatibilityWarning(const SourceLocation & loc,
  ConversionRank rank, const Expr * from, const Type * to) {
  DASSERT(!isErrorResult(from));
  if (isConversionWarning(rank)) {
    diag.error(loc) << Format_Verbose << compatibilityError(rank) <<
        " converting " << from << " from " << from->type() << " to " << to;
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

void typeLinkageName(std::string & out, const TypeRef & ty) {
  typeLinkageName(out, ty.type());
}

// Given a type, append the linkage name of that type to the output buffer.
void typeLinkageName(std::string & out, const Type * ty) {
  ty = dealias(ty);
  if (TypeDefn * td = ty->typeDefn()) {
    out.append(td->linkageName());
  } else if (const FunctionType * ftype = dyn_cast<FunctionType>(ty)) {
    out.append("fn");
    if (!ftype->params().empty()) {
      out.append("(");
      const ParameterList & params = ftype->params();
      for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
        if (it != params.begin()) {
          out.append(",");
        }

        typeLinkageName(out, (*it)->type());
        if ((*it)->isVariadic()) {
          out.append("...");
        }
      }
      out.append(")");
    }

    if (ftype->returnType().isNonVoidType()) {
      out.append("->");
      typeLinkageName(out, ftype->returnType());
    }
  } else if (const TupleType * ttype = dyn_cast<TupleType>(ty)) {
    for (TypeRefList::const_iterator it = ttype->begin(); it != ttype->end(); ++it) {
      if (it != ttype->begin()) {
        out.append(",");
      }

      typeLinkageName(out, *it);
    }
  } else if (const UnionType * utype = dyn_cast<UnionType>(ty)) {
    for (TypeRefList::const_iterator it = utype->members().begin(); it != utype->members().end(); ++it) {
      if (it != utype->members().begin()) {
        out.append("|");
      }

      typeLinkageName(out, *it);
    }
  } else if (const AddressType * mat = dyn_cast<AddressType>(ty)) {
    out.append("__Address[");
    typeLinkageName(out, mat->typeParam(0));
    out.append("]");
  } else if (const PointerType * npt = dyn_cast<PointerType>(ty)) {
    out.append("__Pointer[");
    typeLinkageName(out, npt->typeParam(0));
    out.append("]");
  } else {
    diag.error() << "Type: " << ty;
    DFAIL("Can't compute linkage name of type");
  }
}

/*void typeLinkageName(std::string & out, TupleType * tv) {
  for (TupleType::iterator it = tv->begin(); it != tv->end(); ++it) {
    if (it != tv->begin()) {
      out.append(",");
    }

    typeLinkageName(out, *it);
  }
}*/

// -------------------------------------------------------------------
// Represents a type conversion operation.
Conversion::Conversion(const Type * from)
  : fromType(from)
  , fromValue(NULL)
  , resultValue(NULL)
  , options(0)
{}

Conversion::Conversion(Expr * from)
  : fromType(from->type())
  , fromValue(from)
  , resultValue(NULL)
  , options(0)
{}

Conversion::Conversion(Expr * from, Expr ** to, int opts)
  : fromType(from->type())
  , fromValue(from)
  , resultValue(to)
  , options(opts)
{}

const Type * Conversion::getFromType() const {
  const Type * ty = dealias(fromType);
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

TypeRef Type::typeParam(int index) const {
  diag.debug() << "Type " << this << " does not have type parameters.";
  DFAIL("No type params");
}

ConversionRank Type::convert(const Conversion & cn) const {
  ConversionRank rank;
  if (cn.resultValue == NULL) {
    // Ask the constraint if we can convert to this type. Most types don't
    // know about constraints, but constraints know about most types.
    rank = cn.fromType->convertTo(this);
    if (rank != Incompatible) {
      return rank;
    }
  }

  rank = convertImpl(cn);

  if (rank == Incompatible && (cn.options & Conversion::Coerce) && !cn.resultValue) {
    if (const CompositeType * ctype = dyn_cast<CompositeType>(this)) {
      if (!ctype->passes().isFinished(CompositeType::ConverterPass)) {
        diag.warn() << "Converter pass for " << ctype << " not done.";
      }

      if (!ctype->coercers().empty()) {
        const MethodList & coercers = ctype->coercers();
        ConversionRank bestRank = Incompatible;
        for (MethodList::const_iterator it = coercers.begin(); it != coercers.end(); ++it) {
          const FunctionType * fnType = (*it)->functionType();
          rank = std::min(
              fnType->param(0)->type().canConvert(cn.fromType),
              canConvert(fnType->returnType().type()));
          bestRank = std::max(bestRank, rank);
        }

        rank = bestRank;
      }
    }
  }

  return rank;
}

ConversionRank Type::canConvert(Expr * fromExpr, int options) const {
  return convert(Conversion(fromExpr).setOption(options));
}

ConversionRank Type::canConvert(const Type * fromType, int options) const {
  return convert(Conversion(fromType).setOption(options));
}

Expr * Type::implicitCast(const SourceLocation & loc, Expr * from, int options) const {
  Expr * result = NULL;
  ConversionRank tc = convert(Conversion(from, &result, options));
  compatibilityWarning(loc, tc, from, this);
  DASSERT(tc == Incompatible || result != NULL);
  return result;
}

Expr * Type::explicitCast(const SourceLocation & loc, Expr * from, int options) const {
  Expr * result = NULL;
  ConversionRank tc = convert(Conversion(from, &result, options));
  if (tc == Incompatible) {
    compatibilityWarning(loc, tc, from, this);
  }
  return result;
}

const Type * Type::selectLessSpecificType(const Type * type1, const Type * type2) {
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

bool Type::equivalent(const Type * type1, const Type * type2) {
  while (const PatternValue * pval = dyn_cast<PatternValue>(type1)) {
    type1 = pval->value();
    if (type1 == NULL) {
      return false;
    }
  }

  while (const PatternValue * pval = dyn_cast<PatternValue>(type2)) {
    type2 = pval->value();
    if (type2 == NULL) {
      return false;
    }
  }

  if (type1 == type2) {
    return true;
  }

  // Compare the ASTs to see if they derive from the same original symbol.
  if (type1->typeDefn() != NULL &&
      type2->typeDefn() != NULL &&
      type1->typeDefn()->ast() != NULL &&
      type1->typeDefn()->ast() == type2->typeDefn()->ast()) {

    // Now test the type parameters to see if they are also equivalent.
    const TypeDefn * d1 = type1->typeDefn();
    const TypeDefn * d2 = type2->typeDefn();
    const TypeRefList * type1Params = NULL;
    const TypeRefList * type2Params = NULL;

    if (d1->isTemplate()) {
      type1Params = &d1->templateSignature()->typeParams()->members();
    } else if (d1->isTemplateInstance()) {
      type1Params = &d1->templateInstance()->typeArgs()->members();
    }

    if (d2->isTemplate()) {
      type2Params = &d2->templateSignature()->typeParams()->members();
    } else if (d2->isTemplateInstance()) {
      type2Params = &d2->templateInstance()->typeArgs()->members();
    }

    if (type1Params == type2Params) {
      return true;
    }

    if (type1Params == NULL ||
        type2Params == NULL ||
        type1Params->size() != type2Params->size()) {
      return false;
    }

    size_t numParams = type1Params->size();
    for (size_t i = 0; i < numParams; ++i) {
      if (!equivalent((*type1Params)[i], (*type2Params)[i])) {
        return false;
      }
    }

    return true;
  }

  return false;
}

bool Type::equivalent(const TypeRef & type1, const TypeRef & type2) {
  return type1.modifiers() == type2.modifiers() && equivalent(type1.type(), type2.type());
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
    return tinst->typeArgs()->size();
  }

  return 0;
}

TypeRef DeclaredType::typeParam(int index) const {
  TemplateSignature * tsig = defn_->templateSignature();
  if (tsig != NULL) {
    return tsig->typeParam(index);
  }

  TemplateInstance * tinst = defn_->templateInstance();
  if (tinst != NULL) {
    return tinst->typeArg(index);
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
// TypeRef

const Type * TypeRef::dealias() const {
  return tart::dealias(type_);
}

Type * TypeRef::dealias() {
  return tart::dealias(type_);
}

bool TypeRef::isSubtype(const TypeRef & other) const {
  if (type_ != NULL && other.type_ != NULL && type_->isSubtype(other.type_)) {
    // If the other type has any modifier bits that this one does not have, then
    // it's not a subtype.
    return (~modifiers_ & other.modifiers_) == 0;
  }

  return false;
}

Expr * TypeRef::implicitCast(const SourceLocation & loc, Expr * from, int options) const {
  return type_->implicitCast(loc, from, options);
}

Expr * TypeRef::explicitCast(const SourceLocation & loc, Expr * from, int options) const {
  return type_->explicitCast(loc, from, options);
}

ConversionRank TypeRef::convert(const Conversion & conversion) const {
  return type_->convert(conversion);
}

ConversionRank TypeRef::canConvert(Expr * fromExpr, int options) const {
  return type()->canConvert(fromExpr, options);
}

ConversionRank TypeRef::canConvert(const Type * fromType, int options) const {
  return type()->canConvert(fromType, options);
}

ConversionRank TypeRef::canConvert(const TypeRef & fromType, int options) const {
  return type()->canConvert(fromType.type(), options);
}

FormatStream & operator<<(FormatStream & out, const TypeRef & ref) {
  out << ref.type();
}

// -------------------------------------------------------------------
// Utility functions

const Type * findCommonType(const Type * t0, const Type * t1) {
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
      t = alias->value().type();
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

TypeRef dealias(const TypeRef & tr) {
  return TypeRef(dealias(tr.type()), tr.modifiers());
}

} // namespace tart
