/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Template.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/AmbiguousParameterType.h"
#include "tart/Type/AmbiguousResultType.h"
#include "tart/Type/AmbiguousTypeParamType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/Type.h"
#include "tart/Type/TypeAlias.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeFunction.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Type/TypeRelation.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/UnitType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/Infer/TypeAssignment.h"

#include "llvm/ADT/STLExtras.h"

namespace tart {
namespace TypeConversion {

ConversionRank qualifierConversionRank(unsigned srcQualifiers, unsigned dstQualifiers) {
  if (areQualifiersEquivalent(srcQualifiers, dstQualifiers)) {
    return IdenticalTypes;
  } else if (canAssignQualifiers(srcQualifiers, dstQualifiers)) {
    return ExactConversion;
  } else {
    return QualifierLoss;
  }
}

void assignSrcToDst(QualifiedType srcType, Expr * srcExpr, QualifiedType dstType, Expr **dstExpr) {
  if (dstExpr) {
    if (srcType.qualifiers() != dstType.qualifiers()) {
      *dstExpr = CastExpr::qualCast(srcExpr, dstType);
    } else {
      *dstExpr = srcExpr;
    }
  }
}

#if 0
void assignSrcToDst(
    const Type * srcType, unsigned srcQualifiers, Expr * srcExpr,
    const Type * dstType, unsigned dstQualifiers, Expr **dstExpr) {
  if (dstExpr) {
    if (srcQualifiers != dstQualifiers) {
      *dstExpr = CastExpr::qualCast(srcExpr, QualifiedType(dstType, dstQualifiers));
    } else {
      *dstExpr = srcExpr;
    }
  }
}

void assignSrcToDst(Expr * srcExpr, const Type * dstType, unsigned dstQualifiers, Expr **dstExpr) {
  if (dstExpr) {
    QualifiedType srcType = srcExpr->type();
    if (srcType.qualifiers() != dstType.qualifiers()) {
      *dstExpr = CastExpr::qualCast(srcExpr, QualifiedType(dstType, dstQualifiers));
    } else {
      *dstExpr = srcExpr;
    }
  }
}
#endif

ConversionRank convertToPrimitive(
    const Type * srcType, Expr * srcExpr,
    const PrimitiveType * dstType, Expr ** dstExpr, int options) {
  if (dstType->isUnsizedIntType()) {
    return Incompatible;
  }

  // For now call the old type conversion functions.
  Conversion cn(srcType, options);
  cn.fromValue = srcExpr;
  cn.resultValue = dstExpr;
  ConversionRank rank = dstType->convertImpl(cn);
  if (rank == Incompatible) {
    return Incompatible;
  } else {
    return rank;
  }
}

ConversionRank convertToComposite(
    const Type * srcType, unsigned srcQuals, Expr * srcExpr,
    const CompositeType * dstClass, unsigned dstQuals, Expr ** dstExpr, int options) {
  if (!canAssignQualifiers(srcQuals, dstQuals)) {
    return QualifierLoss;
  }
  if (const CompositeType * srcClass = dyn_cast_or_null<CompositeType>(srcType)) {
    DASSERT(dstClass->passes().isFinished(CompositeType::BaseTypesPass)) <<
        "Base type analysis not finished for " << dstClass;
    DASSERT(srcClass->passes().isFinished(CompositeType::BaseTypesPass)) <<
        "Base type analysis not finished for " << srcClass;
    if (dstClass->typeClass() != Type::Struct && TypeRelation::isSubtype(srcClass, dstClass)) {
      if (srcExpr && dstExpr) {
        *dstExpr = CastExpr::upCast(
            srcExpr, QualifiedType(dstClass, dstQuals))->at(srcExpr->location());
      }
      return ExactConversion;
    }

    /*  } else if (fromType == &NullType::instance) {
    // Conversion from 'null'.
    if (dstClass->isReferenceType()) {
      if (srcExpr && dstExpr) {
        *dstExpr = new ConstantNull(
            srcExpr->location(), dstClass);
      }

      return ExactConversion;
    }*/

    // Check dynamic casts.
    if ((options & CHECKED) && dstClass->isReferenceType() && srcClass->isReferenceType()) {
      if (srcExpr && dstExpr) {
        *dstExpr = CastExpr::tryCast(
            srcExpr, QualifiedType(dstClass, dstQuals))->at(srcExpr->location());
      }

      return NonPreferred;
    }
  } else if (const FunctionType * ftype = dyn_cast<FunctionType>(srcType)) {
    // See if dstClass class implements the Function interface.
    const CompositeType * functionInterface =
        dstClass->findBaseSpecializing(Builtins::typeFunction);
    if (functionInterface != NULL) {
      if (TypeRelation::isEqual(ftype->returnType(), functionInterface->typeParam(0)) &&
          TypeRelation::isEqual(ftype->paramTypes(), functionInterface->typeParam(1))) {

        if (ftype->isStatic()) {
          // We currently only support bound methods.
          DFAIL("Implement conversion of non-method functions");
        }

        if (srcExpr && dstExpr) {
          if (LValueExpr * lval = dyn_cast<LValueExpr>(srcExpr)) {
            if (FunctionDefn * method = dyn_cast<FunctionDefn>(lval->value())) {
              if (method->isIntrinsic()) {
                diag.error(lval) << "Intrinsic methods cannot be called indirectly.";
                return Incompatible;
              } else if (method->isCtor()) {
                diag.error(lval) << "Constructors cannot be called indirectly.";
                return Incompatible;
              }

              DASSERT(lval->base() != NULL);
              *dstExpr = new BoundMethodExpr(
                  lval->location(), lval->base(), method, functionInterface);
              return NonPreferred;
            }
          }

          DFAIL("Implement conversion of non-lvalue functions");
        }

        return NonPreferred;
      }
    }
  }

  if ((options & COERCE) && !dstExpr) {
    if (!dstClass->passes().isFinished(CompositeType::CoercerPass)) {
      diag.warn() << "Converter pass for " << dstClass << " was not done.";
    }

    if (!dstClass->coercers().empty()) {
      const MethodList & coercers = dstClass->coercers();
      ConversionRank bestRank = Incompatible;
      for (MethodList::const_iterator it = coercers.begin(); it != coercers.end(); ++it) {
        const FunctionType * fnType = (*it)->functionType();
        ConversionRank rank = std::min(
            convert(srcType, srcExpr, fnType->param(0)->type(), NULL),
            convert(fnType->returnType(), NULL, dstClass, NULL));
        bestRank = std::max(bestRank, rank);
      }

      // Coerced conversions are at best non-preferred.
      return std::min(bestRank, NonPreferred);
    }
  }

  return Incompatible;
}

ConversionRank convertToEnum(
    const Type * srcType, Expr * srcExpr,
    const EnumType * dstEnumType, Expr ** dstExpr, int options) {
  if (srcType == dstEnumType) {
    assignSrcToDst(srcType, srcExpr, dstEnumType, dstExpr);
    return ExactConversion;
  }

  // An integer 0 can be converted to a flags enum.
  const Type * baseType = dstEnumType->baseType();
  if (dstEnumType->isFlags() && srcType->isIntType() && srcExpr != NULL && srcExpr->isConstant()) {
    if (ConstantInteger * cint = dyn_cast<ConstantInteger>(srcExpr)) {
      if (cint->value()->isNullValue()) {
        return convert(srcType, srcExpr, baseType, dstExpr, options);
      }
    }
  }

  // An integer can be coerced to an enum.
  if (srcType->isIntType() && (options & EXPLICIT)) {
    Expr * expr = NULL;
    ConversionRank rank = convert(srcType, srcExpr, baseType, &expr, COERCE);
    if (rank != Incompatible) {
      if (dstExpr != NULL && expr != NULL) {
        *dstExpr = new CastExpr(Expr::BitCast, srcExpr->location(), dstEnumType, expr);
      }
      return rank;
    }
  }

  // Unboxing
  // TODO: Shouldn't we do more checking than this?
  if ((options & CHECKED) && srcType->isReferenceType()) {
    if (dstExpr != NULL) {
      *dstExpr = new CastExpr(Expr::UnboxCast, srcExpr->location(), dstEnumType, srcExpr);
    }

    return NonPreferred;
  }

  return Incompatible;
}

ConversionRank convertToFunction(
    const Type * srcType, unsigned srcQuals, Expr * srcExpr,
    const FunctionType * dstFnType, unsigned dstQuals, Expr ** dstExpr, int options) {
  if (!canAssignQualifiers(srcQuals, dstQuals)) {
    return QualifierLoss;
  }
  // No conversion for functions, only same types.
  if (TypeRelation::isEqual(srcType, dstFnType)) {
    if (dstExpr) {
      *dstExpr = srcExpr;
    }
    return IdenticalTypes;
  }

  return Incompatible;
}

ConversionRank convertToTuple(
    const Type * srcType, unsigned srcQuals, Expr * srcExpr,
    const TupleType * dstTupleType, unsigned dstQuals, Expr ** dstExpr, int options) {
  const TupleType * srcTupleType = dyn_cast<TupleType>(srcType);
  if (srcTupleType == NULL) {
    return Incompatible;
  }

  if (srcTupleType->numTypeParams() != dstTupleType->numTypeParams()) {
    return Incompatible;
  }

  ExprList args;
  ConversionRank rank = IdenticalTypes;
  size_t fieldCount = srcTupleType->size();
  bool identical = true;
  for (size_t i = 0; i < fieldCount; ++i) {
    QualifiedType srcMemberType = srcTupleType->member(i);
    QualifiedType dstMemberType = dstTupleType->member(i);
    Expr * srcMemberExpr = NULL;
    Expr * dstMemberExpr = NULL;
    if (dstExpr != NULL) {
      if (TupleCtorExpr * tce = dyn_cast<TupleCtorExpr>(srcExpr)) {
        srcMemberExpr = tce->arg(i);
      } else {
        //DFAIL("Implement tuple memberwise conversion");
        return Incompatible;
      }
    }

    ConversionRank fieldRank = convert(
        srcMemberType, srcMemberExpr, dstMemberType, &dstMemberExpr, options | COERCE);
    if (fieldRank == Incompatible) {
      return Incompatible;
    }

    rank = std::min(rank, fieldRank);

    if (srcMemberExpr != NULL) {
      DASSERT(dstMemberExpr != NULL);
      args.push_back(dstMemberExpr);

      if (srcMemberExpr != dstMemberExpr) {
        identical = false;
      }
    }
  }

  if (dstExpr != NULL) {
    DASSERT(srcExpr != NULL);
    if (identical) {
      *dstExpr = srcExpr;
    } else {
      *dstExpr = new TupleCtorExpr(srcExpr->location(), dstTupleType, args);
    }
  }

  if (!canAssignQualifiers(srcQuals, dstQuals)) {
    return QualifierLoss;
  }

  return rank;
}

std::pair<ConversionRank, size_t> selectUnionMember(
    const UnionType * unionType, QualifiedType srcType, Expr * srcExpr, int options) {
  ConversionRank bestRank = Incompatible;
  size_t bestIndex = 0;
  size_t index = 0;
  for (UnionType::const_iterator it = unionType->begin(), itEnd = unionType->end(); it != itEnd;
      ++it, ++index) {
    ConversionRank rank = convert(srcType, srcExpr, *it, NULL, options);
    if (rank > bestRank) {
      bestRank = rank;
      bestIndex = index;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  return std::make_pair(bestRank, bestIndex);
}

ConversionRank convertToUnion(
    QualifiedType srcType, Expr * srcExpr,
    const UnionType * dstUnionType, Expr ** dstExpr, int options) {

  ConversionRank rank;
  size_t index;

  if (Qualified<UnionType> srcUnionType = srcType.dyn_cast<UnionType>()) {
    // Identical unions
    if (TypeRelation::isEqual(&srcUnionType->members(), &dstUnionType->members())) {
      if (dstExpr != NULL) {
        *dstExpr = srcExpr;
      }

      return IdenticalTypes;
    }

    // Compatible unions
    ConversionRank worstRank = IdenticalTypes;
    for (UnionType::const_iterator it = srcUnionType->begin(), itEnd = srcUnionType->end(); it != itEnd; ++it) {
      llvm::tie(rank, index) = selectUnionMember(dstUnionType, *it, NULL, options);
      worstRank = std::min(worstRank, rank);
    }

    if (worstRank != Incompatible && dstExpr != NULL) {
      CastExpr * result = new CastExpr(
          Expr::UnionCtorCast, srcExpr->location(), dstUnionType, srcExpr);
      result->setTypeIndex(index);
      *dstExpr = result;
    }

    return worstRank;
  }

  llvm::tie(rank, index) = selectUnionMember(dstUnionType, srcType, srcExpr, options);
  if (rank != Incompatible && dstExpr != NULL) {
    QualifiedType memberType = dstUnionType->members()[index];
    Expr * intermediateExpr = NULL;
    convert(srcType, srcExpr, memberType, &intermediateExpr, options);
    if (intermediateExpr != NULL) {
      CastExpr * result = new CastExpr(
          Expr::UnionCtorCast, srcExpr->location(), dstUnionType, intermediateExpr);
      result->setTypeIndex(index);
      *dstExpr = result;
    }
  }

  // Since we're converting to a union type, it's not identical.
  // TODO: Don't know if we really need this.
  //if (bestRank == IdenticalTypes) {
  //  bestRank = ExactConversion;
  //}
  return rank;
}

// Determine if an individual array element can legally be cast from one type to another by
// re-interpreting the bits of the data as the destination type - essentially taking the
// address of the source data and casting that pointer to the destination type.
// This means no coercions are allowed.
ConversionRank convertElement(QualifiedType srcType, QualifiedType dstType) {
  DASSERT(!srcType.isNull());
  DASSERT(!dstType.isNull());

  unsigned srcQualifiers = srcType.qualifiers();
  unsigned dstQualifiers = dstType.qualifiers();
  ConversionRank qualifierRank = qualifierConversionRank(srcQualifiers, dstQualifiers);

  // Early out
  if (srcType.unqualified() == dstType.unqualified()) {
    return qualifierRank;
  }

  // Special cases for source types.
  switch (srcType->typeClass()) {
    case Type::Alias:
      return convertElement(srcType.as<TypeAlias>()->value() | srcQualifiers, dstType);

    case Type::AmbiguousParameter:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      QualifiedTypeSet expansion;
      srcType.expand(expansion);
      ConversionRank best = Incompatible;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        best = std::max(best, convertElement(*it | srcQualifiers, dstType));
        if (best == IdenticalTypes) {
          break;
        }
      }

      return best;
    }

    case Type::AmbiguousPhi: {
      QualifiedTypeSet expansion;
      srcType.expand(expansion);
      ConversionRank worst = IdenticalTypes;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        worst = std::min(worst, convertElement(*it | srcQualifiers, dstType));
        if (worst == Incompatible) {
          break;
        }
      }

      return worst;
    }

    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(srcType.unqualified());
      if (ta->value()) {
        return convertElement(ta->value() | srcQualifiers, dstType);
      }

      ConversionRank rank = Incompatible;
      for (ConstraintSet::const_iterator ci = ta->begin(), ciEnd = ta->end(); ci != ciEnd; ++ci) {
        Constraint * c = *ci;
        if (!c->visited() && c->checkProvisions()) {
          c->setVisited(true);
          switch (c->kind()) {
            case Constraint::EXACT:
              rank = std::max(rank, convertElement(c->value() | srcQualifiers, dstType));
              break;

            case Constraint::LOWER_BOUND:
              // In general, the answer to this case is unknowable.
              // For the moment, we'll say 'yes', but with a lower ranking.
              rank = std::max(rank,
                  std::min(NonPreferred, convertElement(c->value() | srcQualifiers, dstType)));
              break;

            case Constraint::UPPER_BOUND:
              rank = std::max(rank, convertElement(c->value() | srcQualifiers, dstType));
              break;
          }
          c->setVisited(false);
        }
      }

      return rank;
    }

    case Type::TypeFnCall: {
      Qualified<TypeFunctionCall> ta = srcType.as<TypeFunctionCall>();
      if (const TypeFunction * tfn = dyn_cast<TypeFunction>(dealias(ta->fnVal()))) {
        return convertElement(tfn->apply(ta->args()) | srcQualifiers, dstType);
      }
      return Incompatible;
    }

    default:
      break;
  }

  switch (dstType->typeClass()) {
    case Type::Alias:
      // Dealias dstType
      return convertElement(srcType, dstType.as<TypeAlias>()->value() | dstQualifiers);

    case Type::AmbiguousParameter:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      QualifiedTypeSet expansion;
      dstType.expand(expansion);
      ConversionRank best = Incompatible;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        best = std::max(best, convertElement(srcType, *it | dstQualifiers));
        if (best == IdenticalTypes) {
          break;
        }
      }

      return best;
    }

    case Type::AmbiguousPhi: {
      QualifiedTypeSet expansion;
      dstType.expand(expansion);
      ConversionRank worst = IdenticalTypes;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        worst = std::min(worst, convertElement(srcType, *it | dstQualifiers));
        if (worst == Incompatible) {
          break;
        }
      }

      return worst;
    }

    case Type::Assignment: {
      Qualified<TypeAssignment> ta = dstType.as<TypeAssignment>();
      if (ta->value()) {
        return convertElement(srcType, ta->value() | dstQualifiers);
      } else {
        ConversionRank rank = Incompatible;
        for (ConstraintSet::const_iterator ci = ta->begin(), ciEnd = ta->end(); ci != ciEnd; ++ci) {
          Constraint * c = *ci;
          if (!c->visited() && c->checkProvisions()) {
            c->setVisited(true);
            switch (c->kind()) {
              case Constraint::EXACT:
                rank = std::max(rank, convertElement(srcType, c->value() | dstQualifiers));
                break;

              case Constraint::LOWER_BOUND:
                // Means T == value or is a supertype of value.
                // Can we convert from srcType to a supertype of 'value'?
                // Should in general be the same as converting to 'value'.
                rank = std::max(rank, convertElement(srcType, c->value() | dstQualifiers));
                break;

              case Constraint::UPPER_BOUND:
                // In general, the answer to this case is unknowable.
                // Means T == value or is a subtype of value.
                rank = std::max(rank, std::min(
                    std::min(rank, NonPreferred),
                    convertElement(srcType, c->value() | dstQualifiers)));
                break;
            }
            c->setVisited(false);
          }
        }
        return rank;
      }
    }

    case Type::TypeFnCall: {
      Qualified<TypeFunctionCall> ta = dstType.as<TypeFunctionCall>();
      if (const TypeFunction * tfn = dyn_cast<TypeFunction>(dealias(ta->fnVal()))) {
        return convertElement(srcType, tfn->apply(ta->args()) | dstQualifiers);
      }
      return Incompatible;
    }

    default:
      // Compare unqualified types for equality.
      if (TypeRelation::isEqual(srcType.unqualified(), dstType.unqualified())) {
        return qualifierRank;
      } else {
        return Incompatible;
      }
  }

  return Incompatible;
}

ConversionRank convertToAddress(
    QualifiedType srcType, Expr * srcExpr,
    Qualified<AddressType> dstType, Expr ** dstExpr, int options) {
  QualifiedType dstElementType = dstType->typeParam(0);
  DASSERT(!dstElementType.isNull());
  if (srcType.isa<AddressType>()) {
    QualifiedType srcElementType = srcType->typeParam(0);
    DASSERT(!srcElementType.isNull());

    ConversionRank rank = std::min(
        convertElement(srcElementType, dstElementType),
        qualifierConversionRank(srcType.qualifiers(), dstType.qualifiers()));
    if (dstExpr && rank > QualifierLoss) {
      assignSrcToDst(srcType, srcExpr, dstType.as<Type>(), dstExpr);
    }
    return rank;
  } else if (srcType->isNullType()) {
    if (dstExpr) {
      *dstExpr = ConstantNull::get(srcExpr->location(), dstType.unqualified());
    }

    return ExactConversion;
  } else {
    return Incompatible;
  }
}

ConversionRank convertToNativeArray(
    QualifiedType srcType, Expr * srcExpr,
    Qualified<NativeArrayType> dstArrayType, Expr ** dstExpr, int options) {
  QualifiedType dstElementType = dstArrayType->elementType();
  if (Qualified<NativeArrayType> srcArrayType = srcType.dyn_cast<NativeArrayType>()) {
    QualifiedType srcElementType = srcArrayType->elementType();
    DASSERT(!srcElementType.isNull());

    if (srcArrayType->size() != dstArrayType->size() /*&& dstArrayType->size() != 0*/) {
      return Incompatible;
    }

    ConversionRank rank = std::min(
        convertElement(srcElementType, dstElementType),
        qualifierConversionRank(srcType.qualifiers(), dstArrayType.qualifiers()));
    if (dstExpr && rank > QualifierLoss) {
      assignSrcToDst(srcType, srcExpr, dstArrayType.as<Type>(), dstExpr);
    }
    return rank;
  } else if (Qualified<CompositeType> cfrom = srcType.dyn_cast<CompositeType>()) {
    // Special case for initializing a native type from an array literal.
    if (cfrom->typeDefn()->ast() == Builtins::typeArray->typeDefn()->ast()) {
      QualifiedType srcElementType = cfrom->typeParam(0);
      if (TypeRelation::isEqual(dstElementType, srcElementType)) {
        if (dstExpr) {
          *dstExpr = srcExpr;
        }
        return IdenticalTypes;
      }
    }
  }
  return Incompatible;
}

ConversionRank convertToFlexibleArray(
    QualifiedType srcType, Expr * srcExpr,
    Qualified<FlexibleArrayType> dstArrayType, Expr ** dstExpr, int options) {
  QualifiedType dstElementType = dstArrayType->elementType();
  if (Qualified<FlexibleArrayType> srcArrayType = srcType.dyn_cast<FlexibleArrayType>()) {
    QualifiedType srcElementType = srcArrayType->elementType();
    DASSERT(!srcElementType.isNull());

    // For flex arrays, the element type must be the same.
    ConversionRank rank = std::min(
        convertElement(srcElementType, dstElementType),
        qualifierConversionRank(srcType.qualifiers(), dstArrayType.qualifiers()));
    if (dstExpr && rank > QualifierLoss) {
      assignSrcToDst(srcType, srcExpr, dstArrayType.as<Type>(), dstExpr);
    }
    return rank;
  }
  return Incompatible;
}

ConversionRank convertToTypeLiteral(
    const Type * srcType, Expr * srcExpr,
    const TypeLiteralType * dstLitType, Expr ** dstExpr, int options) {
  if (isa<TypeLiteralType>(srcType)) {
    DASSERT(!srcType->typeParam(0).isNull());
    if (TypeRelation::isEqual(srcType->typeParam(0), dstLitType->typeParam(0))) {
      if (dstExpr) {
        *dstExpr = srcExpr;
      }
      return IdenticalTypes;
    }
  }
  return Incompatible;
}

ConversionRank convert(
    QualifiedType srcType, Expr * srcExpr,
    QualifiedType dstType, Expr ** dstExpr, int options) {

  DASSERT(!srcType.isNull());
  DASSERT(!dstType.isNull());

  // Early out
  if (srcType.unqualified() == dstType.unqualified()) {
    if (areQualifiersEquivalent(srcType.qualifiers(), dstType.qualifiers()) || (options & UNQUAL)) {
      assignSrcToDst(srcType, srcExpr, dstType, dstExpr);
      return IdenticalTypes;
    } else if (canAssignQualifiers(srcType.qualifiers(), dstType.qualifiers())) {
      assignSrcToDst(srcType, srcExpr, dstType, dstExpr);
      return ExactConversion;
    }
  }

  unsigned srcQualifiers = srcType.qualifiers();

  // Special cases for source types.
  switch (srcType->typeClass()) {
    case Type::Union: {
      const UnionType * ut = static_cast<const UnionType *>(srcType.type());
      if (ut->isSingleOptionalType() && dstType->isReferenceType()) {
        // Find the single optional type.
        const Type * memberType = ut->getFirstNonVoidType();
        DASSERT(memberType != NULL);
        ConversionRank rank = convert(memberType, srcExpr, dstType, dstExpr, options);
        if (rank != Incompatible && dstExpr != NULL) {
          *dstExpr = new CastExpr(Expr::CheckedUnionMemberCast, SourceLocation(), dstType, srcExpr);
        }

        return rank == IdenticalTypes ? ExactConversion : rank;
      }

      // Fall through
      break;
    }

    case Type::Alias:
      // Dealias srcType
      return convert(
          srcType.as<TypeAlias>()->value() | srcQualifiers, srcExpr,
          dstType, dstExpr, options);

    case Type::AmbiguousParameter:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      // Makes no sense to return a value when converting src a constraint.
      if (dstExpr != NULL) {
        return Incompatible;
      }

      QualifiedTypeSet expansion;
      srcType.expand(expansion);
      ConversionRank best = Incompatible;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        best = std::max(best, convert(*it | srcQualifiers, srcExpr, dstType, NULL, options));
        if (best == IdenticalTypes) {
          break;
        }
      }

      return best;
    }

    case Type::AmbiguousPhi: {
      // Makes no sense to return a value when converting from a constraint.
      if (dstExpr != NULL) {
        return Incompatible;
      }

      QualifiedTypeSet expansion;
      srcType.expand(expansion);
      ConversionRank worst = IdenticalTypes;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        worst = std::min(
            worst, convert(*it | srcQualifiers, srcExpr, dstType, NULL, options));
        if (worst == Incompatible) {
          break;
        }
      }

      return worst;
    }

    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(srcType.type());
      if (ta->value()) {
        return convert(ta->value() | srcQualifiers, srcExpr, dstType, dstExpr, options);
      }

      if (dstExpr != NULL) {
        return Incompatible;
      }

      ConversionRank rank = Incompatible;
      for (ConstraintSet::const_iterator ci = ta->begin(), ciEnd = ta->end(); ci != ciEnd; ++ci) {
        Constraint * c = *ci;
        if (!c->visited() && c->checkProvisions()) {
          c->setVisited(true);
          switch (c->kind()) {
            case Constraint::EXACT:
              rank = std::max(rank,
                  convert(c->value() | srcQualifiers, srcExpr, dstType, dstExpr, options));
              break;

            case Constraint::LOWER_BOUND:
              // In general, the answer to this case is unknowable.
              // For the moment, we'll say 'yes', but with a lower ranking.
              rank = std::max(rank, std::min(
                  NonPreferred,
                  convert(c->value() | srcQualifiers, srcExpr, dstType, dstExpr, options)));
              break;

            case Constraint::UPPER_BOUND:
              rank = std::max(rank,
                  convert(c->value() | srcQualifiers, srcExpr, dstType, dstExpr, options));
              break;
          }
          c->setVisited(false);
        }
      }

      return rank;
    }

    case Type::TypeFnCall: {
      Qualified<TypeFunctionCall> ta = srcType.as<TypeFunctionCall>();
      if (const TypeFunction * tfn = dyn_cast<TypeFunction>(dealias(ta->fnVal()))) {
        return convert(
            tfn->apply(ta->args()) | srcQualifiers, srcExpr,
            dstType, dstExpr, options);
      }
      return Incompatible;
    }

    default:
      break;
  }

  unsigned dstQualifiers = dstType.qualifiers();
  switch (dstType->typeClass()) {
    case Type::Alias:
      // Dealias dstType
      return convert(
          srcType, srcExpr,
          dstType.as<TypeAlias>()->value() | dstQualifiers, dstExpr, options);

    case Type::Primitive:
      return convertToPrimitive(
          srcType.unqualified(), srcExpr,
          static_cast<const PrimitiveType *>(dstType.unqualified()), dstExpr, options);

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      return convertToComposite(
          srcType.unqualified(), srcQualifiers, srcExpr,
          static_cast<const CompositeType *>(dstType.type()), dstQualifiers, dstExpr,
          options);

    case Type::Enum:
      return convertToEnum(srcType.unqualified(), srcExpr,
          dstType.as<EnumType>().unqualified(), dstExpr, options);

    case Type::Function:
      return convertToFunction(srcType.unqualified(), srcQualifiers, srcExpr,
          dstType.as<FunctionType>().unqualified(), dstQualifiers, dstExpr, options);

    case Type::Tuple:
      return convertToTuple(srcType.unqualified(), srcQualifiers, srcExpr,
          dstType.as<TupleType>().unqualified(), dstQualifiers, dstExpr, options);

    case Type::Union:
      return convertToUnion(srcType, srcExpr,
          dstType.as<UnionType>().type(), dstExpr, options);

    case Type::NAddress:
      return convertToAddress(srcType, srcExpr,
          dstType.as<AddressType>(), dstExpr, options);

    case Type::NArray:
      return convertToNativeArray(srcType, srcExpr,
          dstType.as<NativeArrayType>(), dstExpr, options);

    case Type::FlexibleArray:
      return convertToFlexibleArray(srcType, srcExpr,
          dstType.as<FlexibleArrayType>(), dstExpr, options);

    case Type::Unit:
      DASSERT(dstExpr == NULL);
      return TypeRelation::isEqual(srcType, dstType) ? IdenticalTypes : Incompatible;

    case Type::TypeVar:
      DASSERT(dstExpr == NULL) << "Shouldn't be attempting to call convert on a TypeVar.";
      return NonPreferred;

    case Type::TypeLiteral:
      return convertToTypeLiteral(srcType.type(), srcExpr,
          dstType.as<TypeLiteralType>().type(), dstExpr, options);

    case Type::AmbiguousParameter:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      // Makes no sense to return a value when converting to a constraint.
      if (dstExpr != NULL) {
        return Incompatible;
      }

      QualifiedTypeSet expansion;
      dstType.expand(expansion);
      ConversionRank best = Incompatible;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        best = std::max(best, convert(srcType, srcExpr, *it | dstQualifiers, NULL, options));
        if (best == IdenticalTypes) {
          break;
        }
      }

      return best;
    }

    case Type::AmbiguousPhi: {
      // Makes no sense to return a value when converting to a constraint.
      if (dstExpr != NULL) {
        return Incompatible;
      }

      QualifiedTypeSet expansion;
      dstType.expand(expansion);
      ConversionRank worst = IdenticalTypes;
      for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
        worst = std::min(worst, convert(srcType, srcExpr, *it | dstQualifiers, NULL, options));
        if (worst == Incompatible) {
          break;
        }
      }

      return worst;
    }

    case Type::Assignment: {
      Qualified<TypeAssignment> ta = dstType.as<TypeAssignment>();
      if (ta->value()) {
        return convert(srcType, srcExpr, ta->value() | dstQualifiers, dstExpr, options);
      } else {
        ConversionRank rank = Incompatible;
        for (ConstraintSet::const_iterator ci = ta->begin(), ciEnd = ta->end(); ci != ciEnd; ++ci) {
          Constraint * c = *ci;
          if (!c->visited() && c->checkProvisions()) {
            c->setVisited(true);
            switch (c->kind()) {
              case Constraint::EXACT:
                rank = std::max(rank,
                    convert(srcType, srcExpr, c->value() | dstQualifiers, dstExpr, options));
                break;

              case Constraint::LOWER_BOUND:
                // Means T == value or is a supertype of value.
                // Can we convert from srcType to a supertype of 'value'?
                // Should in general be the same as converting to 'value'.
                rank = std::max(rank,
                    convert(srcType, srcExpr, c->value() | dstQualifiers, dstExpr, options));
                break;

              case Constraint::UPPER_BOUND:
                // In general, the answer to this case is unknowable.
                // Means T == value or is a subtype of value.
                rank = std::max(rank, std::min(
                    std::min(rank, NonPreferred),
                    convert(srcType, srcExpr, c->value() | dstQualifiers, dstExpr, options)));
                break;
            }
            c->setVisited(false);
          }
        }
        return rank;
      }
    }

    case Type::TypeFnCall: {
      Qualified<TypeFunctionCall> ta = dstType.as<TypeFunctionCall>();
      if (const TypeFunction * tfn = dyn_cast<TypeFunction>(dealias(ta->fnVal()))) {
        return convert(srcType, srcExpr,
            tfn->apply(ta->args()) | dstQualifiers, dstExpr, options);
      }
      return Incompatible;
    }

    case Type::TypeFnQual:
    case Type::KindCount:
      DASSERT(false) << "Type class " << dstType->typeClass() << " not supported by convert()";
      break;
  }

  return Incompatible;
}

} // namespace TypeConversion
} // namespace tart
