/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/StaticType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/AST/ASTNode.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsics.h"
#include "llvm/ADT/APInt.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"

namespace tart {

using llvm::APInt;

PrimitiveType * PrimitiveType::primitiveTypeList;

void PrimitiveType::initPrimitiveTypes(Module * module) {
  for (PrimitiveType * ptype = primitiveTypeList; ptype != NULL; ptype = ptype->nextType()) {
    ptype->init();
    TypeDefn * de = ptype->typeDefn();
    de->setQualifiedName(de->name());
    if (!ptype->isUnsizedIntType()) {
      de->addTrait(Defn::Singular);
    }

    module->addMember(de);
  }
}

/// -------------------------------------------------------------------
/// Primitive types
PrimitiveType::PrimitiveType(TypeDefn * de) :
  DeclaredType(Type::Primitive, de, &Builtins::module, Shape_Primitive) {
  nextType_ = primitiveTypeList;
  primitiveTypeList = this;
}

const llvm::Type * PrimitiveType::createIRType() const {
  diag.fatal() << "Attempt to create IR type for " << this;
  DFAIL("IllegalState");
}

const Type * PrimitiveType::derefEnumType(const Type * in) {
  while (const EnumType * etype = dyn_cast<EnumType> (in)) {
    DASSERT(etype->baseType() != NULL)
;    in = etype->baseType();
  }

  return in;
}

ConversionRank PrimitiveType::convertToInteger(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (fromType == NULL) {
    return Incompatible;
  }

  if (fromType == this) {
    if (cn.resultValue) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  if (ConstantExpr * cbase = dyn_cast_or_null<ConstantExpr>(cn.fromValue)) {
    return convertConstantToInteger(cn);
  }

  TypeId dstId = this->typeId();
  uint32_t dstBits = this->numBits();

  fromType = derefEnumType(fromType);

  if (const PrimitiveType * fromPType = dyn_cast<PrimitiveType>(fromType)) {
    TypeId srcId = fromPType->typeId();
    uint32_t srcBits = fromPType->numBits();

    if (isUnsignedIntegerTypeId(dstId)) {
      if (isUnsignedIntegerTypeId(srcId)) {
        ConversionRank result = ExactConversion;
        if (srcBits > dstBits) {
          result = Truncation;
        } else if (srcId == TypeId_Char || dstId == TypeId_Char) {
          // Converting from char to unsigned int is non-preferred.
          result = NonPreferred;
        }

        //if ((srcId == TypeId_Char && dstId == TypeId_UInt32) ||
        //    (srcId == TypeId_UInt32 && dstId == TypeId_Char)) {
        //  result = NonPreferred;
        //}

        if (cn.fromValue && cn.resultValue) {
          // Either truncate or z-extend
          *cn.resultValue = new CastExpr(
              srcBits > dstBits ? Expr::Truncate : Expr::ZeroExtend,
              cn.fromValue->location(), this, cn.fromValue);
        }

        return result;
      } else if (isSignedIntegerTypeId(srcId)) {
        if (cn.fromValue && cn.resultValue) {
          // Either truncate or s-extend
          *cn.resultValue = new CastExpr(
              srcBits > dstBits ? Expr::Truncate : Expr::SignExtend,
              cn.fromValue->location(), this, cn.fromValue);
        }

        return SignedUnsigned;
      } else if (isFloatingTypeId(srcId)) {
        if (cn.fromValue && cn.resultValue) {
          // Convert from float
          DFAIL("Implement");
        }

        return PrecisionLoss;
      } else if (srcId == TypeId_Bool) {
        if (cn.fromValue && cn.resultValue) {
          // Convert from bool
          DFAIL("Implement");
        }

        //return BoolToInteger;
        return ExactConversion;
      }
    } else if (isSignedIntegerTypeId(dstId)) {
      if (isSignedIntegerTypeId(srcId)) {
        ConversionRank result = ExactConversion;
        if (srcBits > dstBits) {
          result = Truncation;
        }

        if (cn.fromValue && cn.resultValue) {
          // Either truncate or s-extend
          *cn.resultValue = new CastExpr(
              srcBits > dstBits ? Expr::Truncate : Expr::SignExtend,
              cn.fromValue->location(), this, cn.fromValue);
        }

        return result;
      } else if (isUnsignedIntegerTypeId(srcId)) {
        ConversionRank result = NonPreferred;
        if (srcBits > dstBits) {
          result = Truncation;
        } else if (srcBits == dstBits) {
          result = SignedUnsigned;
        }

        if (cn.fromValue && cn.resultValue) {
          // Either truncate or z-extend
          *cn.resultValue = new CastExpr(
              srcBits > dstBits ? Expr::Truncate : Expr::ZeroExtend,
              cn.fromValue->location(), this, cn.fromValue);
        }

        return result;
      } else if (isFloatingTypeId(srcId)) {
        if (cn.fromValue && cn.resultValue) {
          //return new CastExpr(llvm::Instruction::FPToSI, expr, this);
          DFAIL("Implement");
        }

        return Truncation;
      } else if (srcId == TypeId_Bool) {
        if (cn.fromValue && cn.resultValue) {
          //return new CastExpr(llvm::Instruction::SExt, expr, this);
          DFAIL("Implement");
        }

        return ExactConversion;
        //return BoolToInteger;
      }
    }

    if (srcId == TypeId_UnsizedInt) {
      if (cn.fromValue && cn.resultValue) {
        //return new CastExpr(llvm::Instruction::SExt, expr, this);
        diag.warn() << "unimplemented conversion from " << fromType << " to " << this;
        DFAIL("Implement");
      }

      // The only way we can get here is via overload selection on a built-in
      // function that has an unsized int return type.
      return NonPreferred;
    }

    if (srcId == TypeId_Bad || srcId == TypeId_Void) {
      return Incompatible;
    }

    diag.fatal() << "cannot convert " << fromType << " to " << this;
    DFAIL("Illegal State");
  } else if ((cn.options & Conversion::Checked) && fromType->isReferenceType()) {
    return convertFromObject(cn);
  } else {
    return Incompatible;
  }
}

ConversionRank PrimitiveType::convertConstantToInteger(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();

  DASSERT(cn.fromValue != NULL);
  DASSERT(cn.fromValue->type()->isEqual(fromType));

  fromType = derefEnumType(fromType);
  TypeId dstId = this->typeId();
  uint32_t dstBits = this->numBits();

  bool dstIsSigned = isSignedIntegerTypeId(dstId);

  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(cn.fromValue)) {
    const PrimitiveType * srcType = cast<PrimitiveType>(fromType);
    TypeId srcId = srcType->typeId();
    if (srcId == TypeId_UnsizedInt) {
      // Convert from 'unsized' integer to sized int.
      return fromUnsizedIntToInt(cint, cn.resultValue);
    } else if (isUnsignedIntegerTypeId(srcId)) {
      // Convert from unsigned int.
      APInt srcVal(cint->value()->getValue());
      uint32_t srcBits = srcVal.getActiveBits();

      if (cn.resultValue) {
        *cn.resultValue = new ConstantInteger(
            cint->location(),
            this,
            cast<llvm::ConstantInt>(llvm::ConstantExpr::getIntegerCast(
                    cint->value(), this->irType(), false)));
      }

      if (srcBits > dstBits) {
        return Truncation;
      }

      // Converting from char to uint32 is non-preferred.
      if ((srcId == TypeId_Char && dstId == TypeId_UInt32) ||
          (srcId == TypeId_UInt32 && dstId == TypeId_Char)) {
        return NonPreferred;
      }

      return ExactConversion;
    } else if (isSignedIntegerTypeId(srcId)) {
      // Convert from signed int.
      APInt srcVal(cint->value()->getValue());
      uint32_t srcBits = srcVal.getMinSignedBits();

      if (cn.resultValue) {
        *cn.resultValue = new ConstantInteger(
            cint->location(),
            this,
            cast<llvm::ConstantInt>(llvm::ConstantExpr::getIntegerCast(
                    cint->value(), this->irType(), true)));
      }

      if (srcBits > dstBits) {
        return Truncation;
      }

      // Converting from char to uint32 is non-preferred.
      if ((srcId == TypeId_SInt32 && dstId == TypeId_Char)) {
        return NonPreferred;
      }

      return ExactConversion;
    } else if (srcId == TypeId_Bool) {
      // Convert to 0 or 1
      if (cn.resultValue) {
        *cn.resultValue = new ConstantInteger(cint->location(), this, cint->value());
      }

      return ExactConversion;
    } else {
      DFAIL("ConstantInteger expression with non-integer type");
    }
  } else if (ConstantFloat * cfloat = dyn_cast<ConstantFloat>(cn.fromValue)) {
    const PrimitiveType * fromType = cast<PrimitiveType>(fromType);
    llvm::ConstantFP * floatVal = cfloat->value();

    if (cn.resultValue) {
      DFAIL("Implement");
    }

    return PrecisionLoss;
  } else {
    // Cannot convert this type of constant to an integer.
    return Incompatible;
  }
}

ConversionRank PrimitiveType::fromUnsizedIntToInt(const ConstantInteger * cint, Expr ** out) const {
  bool dstIsSigned = isSignedIntegerTypeId(typeId());

  // Number of bits needed to hold the integer constant.
  uint32_t bitsNeeded = dstIsSigned
  ? cint->value()->getValue().getMinSignedBits()
  : cint->value()->getValue().getActiveBits();
  uint32_t srcBits = cint->value()->getBitWidth();
  ConversionRank rank;
  unsigned castOp;

  uint32_t dstBits = numBits();
  if (bitsNeeded > dstBits) {
    castOp = llvm::Instruction::Trunc;
    rank = Truncation;
  } else {
    if (srcBits > dstBits) {
      castOp = llvm::Instruction::Trunc;
    } else if (srcBits < dstBits) {
      castOp = dstIsSigned ? llvm::Instruction::SExt : llvm::Instruction::ZExt;
    } else {
      castOp = llvm::Instruction::BitCast;
    }

    if (!dstIsSigned) {
      if (cint->value()->getValue().isNegative()) {
        rank = SignedUnsigned;
      } else {
        rank = ExactConversion;
        //rank = ConstSignChange;
      }
    } else {
      rank = ExactConversion;
    }
  }

  if (out != NULL) {
    llvm::Constant * castVal = llvm::ConstantExpr::getCast(castOp, cint->value(), irType_);
    *out = new ConstantInteger(
        cint->location(), this,
        cast<llvm::ConstantInt>(castVal));
  }

  return rank;
}

ConversionRank PrimitiveType::convertToFloat(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (fromType == NULL) {
    return Incompatible;
  }

  if (fromType == this) {
    if (cn.resultValue) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  TypeId dstId = this->typeId();
  uint32_t dstBits = this->numBits();

  if (ConstantExpr * cbase = dyn_cast_or_null<ConstantExpr>(cn.fromValue)) {
    return convertConstantToFloat(cn);
  }

  if (const PrimitiveType * fromPType = dyn_cast<PrimitiveType>(fromType)) {
    TypeId srcId = fromPType->typeId();
    uint32_t srcBits = fromPType->numBits();

    if (isFloatingTypeId(srcId)) {
      if (cn.fromValue && cn.resultValue) {
        // float convert
        *cn.resultValue = new CastExpr(
            srcBits > dstBits ? Expr::Truncate : Expr::SignExtend,
            cn.fromValue->location(), this, cn.fromValue);
      }

      DASSERT(srcBits != dstBits);
      return srcBits > dstBits ? Truncation : ExactConversion;
    } else if (isUnsignedIntegerTypeId(srcId)) {
      ConversionRank result = NonPreferred;
      if (srcBits > 48 || (dstId == TypeId_Float && srcBits > 24)) {
        ConversionRank result = PrecisionLoss;
      }

      if (cn.fromValue && cn.resultValue) {
        // float convert
        *cn.resultValue = new CastExpr(Expr::IntToFloat,
            cn.fromValue->location(), this, cn.fromValue);
      }

      return result;
    } else if (isSignedIntegerTypeId(srcId)) {
      ConversionRank result = NonPreferred;
      if (srcBits > 48 || (dstId == TypeId_Float && srcBits > 24)) {
        ConversionRank result = PrecisionLoss;
      }

      if (cn.fromValue && cn.resultValue) {
        // float convert
        *cn.resultValue = new CastExpr(Expr::IntToFloat,
            cn.fromValue->location(), this, cn.fromValue);
      }

      return result;
    }
  } else if ((cn.options & Conversion::Checked) && fromType->isReferenceType()) {
    return convertFromObject(cn);
  }

  return Incompatible;
}

ConversionRank PrimitiveType::convertConstantToFloat(const Conversion & cn) const {

  const Type * fromType = cn.getFromType();

  DASSERT(cn.fromValue != NULL);
  DASSERT(cn.fromValue->type()->isEqual(fromType));

  TypeId dstId = this->typeId();
  uint32_t dstBits = this->numBits();

  bool dstIsSigned = isSignedIntegerTypeId(dstId);
  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(cn.fromValue)) {
    const PrimitiveType * srcType = cast<PrimitiveType>(fromType);
    TypeId srcId = srcType->typeId();

    if (srcId == TypeId_UnsizedInt) {
      // Convert from 'unsized' integer to sized int.
      //return fromUnsizedIntToInt(cint, dstBits, cn.resultValue);
      return fromUnsizedIntToFloat(cint, cn.resultValue);
    } else if (isUnsignedIntegerTypeId(srcId)) {
      // Convert from unsigned int.
      APInt srcVal(cint->value()->getValue());
      uint32_t srcBits = srcVal.getActiveBits();

      if (cn.resultValue) {
        *cn.resultValue = new ConstantFloat(
            cint->location(), this,
            cast<llvm::ConstantFP>(llvm::ConstantExpr::getUIToFP(cint->value(), irType())));
      }

      if (srcBits > 48 || (dstId == TypeId_Float && srcBits > 24)) {
        return PrecisionLoss;
      } else {
        return ExactConversion;
      }
    } else if (isSignedIntegerTypeId(srcId)) {
      // Convert from signed int.
      APInt srcVal(cint->value()->getValue());
      uint32_t srcBits = srcVal.getMinSignedBits();

      if (cn.resultValue) {
        *cn.resultValue = new ConstantFloat(
            cint->location(), this,
            cast<llvm::ConstantFP>(llvm::ConstantExpr::getSIToFP(cint->value(), irType())));
      }

      if (srcBits > 48 || (dstId == TypeId_Float && srcBits > 24)) {
        return PrecisionLoss;
      } else {
        return ExactConversion;
      }
    } else {
      DFAIL("ConstantInteger expression with non-integer type");
    }
  } else if (ConstantFloat * cfloat = dyn_cast<ConstantFloat>(cn.fromValue)) {
    const PrimitiveType * ptype = cast<PrimitiveType>(fromType);
    uint32_t srcBits = ptype->numBits();

    if (cn.resultValue) {
      *cn.resultValue = new ConstantFloat(
          cfloat->location(), this,
          cast<llvm::ConstantFP>(llvm::ConstantExpr::getFPCast(cfloat->value(), irType())));
    }

    if (srcBits < dstBits) {
      return PrecisionLoss;
    } else {
      return ExactConversion;
    }
  } else {
    // Cannot convert this type of constant to an integer.
    return Incompatible;
  }
}

ConversionRank PrimitiveType::fromUnsizedIntToFloat(const ConstantInteger * cint,
    Expr ** out) const {

  // Number of bits needed to hold the integer constant.
  uint32_t bitsNeeded = cint->value()->getValue().getMinSignedBits();
  TypeId dstId = typeId();

  if (out != NULL) {
    //dstValue = ConstantExpr::getIntegerCast(ci, this->irType(),
    //srcIsSigned);
    //*cn.resultValue = new ConstantInteger(cint->location(),
    //    this, cint->value());
    DFAIL("Implement");
  }

  if (bitsNeeded > 48 || (dstId == TypeId_Float && bitsNeeded > 24)) {
    return PrecisionLoss;
  } else {
    return NonPreferred;
  }
}

ConversionRank PrimitiveType::convertToBool(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (fromType == NULL) {
    return Incompatible;
  }

  if (fromType == this) {
    if (cn.resultValue) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  if (ConstantExpr * cbase = dyn_cast_or_null<ConstantExpr>(cn.fromValue)) {
    return convertConstantToBool(cn);
  }

  if (const PrimitiveType * fromPType = dyn_cast<PrimitiveType>(fromType)) {
    TypeId srcId = fromPType->typeId();
    if (isIntegerTypeId(srcId)) {
      if (cn.fromValue && cn.resultValue) {
        // Compare with 0.
        *cn.resultValue = new CompareExpr(SourceLocation(), llvm::CmpInst::ICMP_NE, cn.fromValue,
            ConstantInteger::get(SourceLocation(), cn.fromValue->type(), 0));
      }

      return IntegerToBool;
    }

    return Incompatible;
  } else if ((cn.options & Conversion::Checked) && fromType->isReferenceType()) {
    return convertFromObject(cn);
  } else {
    return Incompatible;
  }
}

ConversionRank PrimitiveType::convertConstantToBool(const Conversion & cn) const {
  using namespace llvm;

  const Type * fromType = cn.getFromType();
  DASSERT(cn.fromValue != NULL);
  DASSERT(cn.fromValue->type()->isEqual(fromType));
  if (ConstantInteger * cint = dyn_cast<ConstantInteger>(cn.fromValue)) {
    const PrimitiveType * srcType = cast<PrimitiveType>(fromType);
    TypeId srcId = srcType->typeId();
    DASSERT(isIntegerTypeId(srcId) || srcId == TypeId_UnsizedInt);

    if (cn.resultValue) {
      Constant * cival = cint->value();
      Constant * czero = Constant::getNullValue(cival->getType());
      *cn.resultValue = new ConstantInteger(
          cint->location(),
          &BoolType::instance,
          cast<ConstantInt>(
              llvm::ConstantExpr::getCompare(ICmpInst::ICMP_NE, cival, czero)));
    }

    return IntegerToBool;
  } else if (ConstantFloat * cfloat = dyn_cast<ConstantFloat>(cn.fromValue)) {
    return Incompatible;
  } else {
    return Incompatible;
  }
}

ConversionRank PrimitiveType::convertFromObject(const Conversion & cn) const {
  if (cn.resultValue != NULL) {
    *cn.resultValue = new CastExpr(Expr::UnboxCast, cn.fromValue->location(),
        this, cn.fromValue);
  }

  return NonPreferred;
}

void PrimitiveType::defineConstant(const char * name, ConstantExpr * value) {
  if (value->type() != this) {
    diag.error() << value->type() << " != " << this;
  }
  DASSERT(value->type() == this);
  VariableDefn * var = new VariableDefn(Defn::Let, NULL, name, value);
  var->setStorageClass(Storage_Static);
  addMember(var);
}

/// -------------------------------------------------------------------
/// Class which defines conversion constructors for primitive types.
template<int to, int from>
class PrimitiveConstructor : public FunctionDefn {
public:
  PrimitiveConstructor() : FunctionDefn(NULL, "create", &StaticFnType1<to, from>::value) {
    setStorageClass(Storage_Static);
  }

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    DASSERT(args.size() == 1);
    Expr * arg = args[0];
    return StaticType<to>::value.explicitCast(loc, arg);
  }

  // Override format so that we can print the type name instead of just 'create'.
  void format(FormatStream & out) const {
    if (out.isVerbose()) {
      out << "def ";
    }

    out << &PrimitiveTypeImpl<TypeId(to)>::instance;
    if (out.getShowType()) {
      out << "(" << &PrimitiveTypeImpl<TypeId(from)>::instance << ")";
      out << " -> " << &PrimitiveTypeImpl<TypeId(to)>::instance;
    }
  }

  static PrimitiveConstructor value;
};

template<int to, int from>
PrimitiveConstructor<to, from> PrimitiveConstructor<to, from>::value;

/// -------------------------------------------------------------------
/// Class which defines a 'toString' function for each primitive type.
template<int kTypeId>
class PrimitiveToString : public FunctionDefn {
public:
  PrimitiveToString() : FunctionDefn(NULL, "toString", &type) {
    setStorageClass(Storage_Instance);
    addTrait(Singular);
    setLinkageName("toString");
  }

  void init() {
    // Can't do this in constructor because it happens too early.
    setIntrinsic(PrimitiveToStringIntrinsic::get("PrimitiveType.toString"));
  }

  static ParameterDefn selfParam;
  static ParameterDefn * params[];
  static FunctionType type;
  static PrimitiveToString value;
};

template<int kTypeId>
ParameterDefn PrimitiveToString<kTypeId>::selfParam(
    NULL,
    "self",
    &StaticType<kTypeId>::value, NULL);

template<int kTypeId>
FunctionType PrimitiveToString<kTypeId>::type(
    &StaticType<TypeId_String>::value, &selfParam, NULL, 0);

template<int kTypeId>
PrimitiveToString<kTypeId> PrimitiveToString<kTypeId>::value;

/// -------------------------------------------------------------------
/// Primitive type: Void

// Yes, void values can be constructed.
class VoidConstructor : public FunctionDefn {
public:
  VoidConstructor() : FunctionDefn(NULL, "create", &StaticFnType0<TypeId_Void>::value) {
    setStorageClass(Storage_Static);
  }

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    DASSERT(args.size() == 0);
    return ConstantNull::get(loc, &VoidType::instance);
  }

  static VoidConstructor value;
};

VoidConstructor VoidConstructor::value;

template<> TypeDefn VoidType::typedefn(&Builtins::module, "void", &VoidType::instance);
template<> ASTBuiltIn VoidType::biDef(&typedefn);
template<> TypeIdSet VoidType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet VoidType::INCLUDES = TypeIdSet::noneOf();

template<> void VoidType::init() {
  irType_ = llvm::StructType::get(llvm::getGlobalContext(), false);
  addMember(&VoidConstructor::value);
}

template<> uint32_t VoidType::numBits() const {
  return 0;
}

template<> ConversionRank
VoidType::convertImpl(const Conversion & cn) const {
  if (cn.getFromType()->isEqual(this)) {
    if (cn.fromValue && cn.resultValue) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  return Incompatible;
}

template<> Expr * VoidType::nullInitValue() const {
  return NULL;
}

/// -------------------------------------------------------------------
/// Primitive type: Bool

template<> TypeDefn BoolType::typedefn(&Builtins::module, "bool", &BoolType::instance);
template<> TypeIdSet BoolType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet BoolType::INCLUDES = TypeIdSet::noneOf();

template<> void BoolType::init() {
  irType_ = llvm::Type::getInt1Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_Bool, TypeId_Bool>::value);
  addMember(&PrimitiveToString<TypeId_Bool>::value);

  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(1), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(1), this));

  PrimitiveToString<TypeId_Bool>::value.init();
}

template<> uint32_t BoolType::numBits() const {
  return 1;
}

template<> ConversionRank
BoolType::convertImpl(const Conversion & cn) const {
  return convertToBool(cn);
}

template<> Expr * BoolType::nullInitValue() const {
  return NULL;
}

/// -------------------------------------------------------------------
/// Primitive type: Char

template<> TypeDefn CharType::typedefn(&Builtins::module, "char", &CharType::instance);
template<> TypeIdSet CharType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet CharType::INCLUDES = TypeIdSet::noneOf();

template<> void CharType::init() {
  irType_ = llvm::Type::getInt32Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_Char, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_Char, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_Char, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_Char, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_Char, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_Char, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_Char>::value);

  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(32), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(32), this));

  PrimitiveToString<TypeId_Char>::value.init();
}

template<> uint32_t CharType::numBits() const {
  return 32;
}

template<> ConversionRank CharType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * CharType::nullInitValue() const {
  return NULL;
}

/// -------------------------------------------------------------------
/// Primitive type: Byte

template<> TypeDefn ByteType::typedefn(&Builtins::module, "byte", &ByteType::instance);
template<> TypeIdSet ByteType::MORE_GENERAL =
    TypeIdSet::of(TypeId_SInt16, TypeId_SInt32, TypeId_SInt64);
template<> TypeIdSet ByteType::INCLUDES = TypeIdSet::noneOf();

template<> void ByteType::init() {
  irType_ = llvm::Type::getInt8Ty(llvm::getGlobalContext());

  // Conversion constructors
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt8, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_SInt8>::value);

  defineConstant("minVal", ConstantInteger::getSigned(llvm::APInt::getSignedMinValue(8), this));
  defineConstant("maxVal", ConstantInteger::getSigned(llvm::APInt::getSignedMaxValue(8), this));

  PrimitiveToString<TypeId_SInt8>::value.init();
}

template<> uint32_t ByteType::numBits() const {
  return 8;
}

template<> ConversionRank
ByteType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * ByteType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Short

template<> TypeDefn ShortType::typedefn(&Builtins::module, "short", &ShortType::instance);
template<> TypeIdSet ShortType::MORE_GENERAL = TypeIdSet::of(TypeId_SInt32, TypeId_SInt64);
template<> TypeIdSet ShortType::INCLUDES = TypeIdSet::of(TypeId_SInt8, TypeId_UInt8);

template<> void ShortType::init() {
  irType_ = llvm::Type::getInt16Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt16, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_SInt16>::value);

  defineConstant("minVal", ConstantInteger::getSigned(llvm::APInt::getSignedMinValue(16), this));
  defineConstant("maxVal", ConstantInteger::getSigned(llvm::APInt::getSignedMaxValue(16), this));

  PrimitiveToString<TypeId_SInt16>::value.init();
}

template<> uint32_t ShortType::numBits() const {
  return 16;
}

template<> ConversionRank
ShortType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * ShortType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Int

template<> TypeDefn IntType::typedefn(&Builtins::module, "int", &IntType::instance);
template<> TypeIdSet IntType::MORE_GENERAL = TypeIdSet::of(TypeId_SInt64);
template<> TypeIdSet IntType::INCLUDES = TypeIdSet::of(
    TypeId_SInt8, TypeId_SInt16, TypeId_UInt8, TypeId_UInt16
);

template<> void IntType::init() {
  irType_ = llvm::Type::getInt32Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt32, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_SInt32>::value);

  defineConstant("minVal", ConstantInteger::getSigned(llvm::APInt::getSignedMinValue(32), this));
  defineConstant("maxVal", ConstantInteger::getSigned(llvm::APInt::getSignedMaxValue(32), this));

  PrimitiveToString<TypeId_SInt32>::value.init();
}

template<> uint32_t IntType::numBits() const {
  return 32;
}

template<> ConversionRank
IntType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * IntType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Long

template<> TypeDefn LongType::typedefn(&Builtins::module, "long", &LongType::instance);
template<> TypeIdSet LongType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet LongType::INCLUDES = TypeIdSet::of(
    TypeId_SInt8, TypeId_SInt16, TypeId_SInt32, TypeId_UInt8, TypeId_UInt16, TypeId_UInt32);

template<> void LongType::init() {
  irType_ = llvm::Type::getInt64Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_SInt64>::value);

  defineConstant("minVal", ConstantInteger::getSigned(llvm::APInt::getSignedMinValue(64), this));
  defineConstant("maxVal", ConstantInteger::getSigned(llvm::APInt::getSignedMaxValue(64), this));

  PrimitiveToString<TypeId_SInt64>::value.init();
}

template<> uint32_t LongType::numBits() const {
  return 64;
}

template<> ConversionRank LongType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * LongType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Long

template<> TypeDefn IntPtrType::typedefn(&Builtins::module, "intptr", &IntPtrType::instance);
template<> TypeIdSet IntPtrType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet IntPtrType::INCLUDES = TypeIdSet::noneOf();
//TypeIdSet::of(
//    TypeId_SInt8, TypeId_SInt16, TypeId_SInt32, TypeId_UInt8, TypeId_UInt16, TypeId_UInt32);

template<> void IntPtrType::init() {
  irType_ = llvm::Type::getInt8PtrTy(llvm::getGlobalContext());
/*  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_SInt64, TypeId_UInt64>::value);
*/  addMember(&PrimitiveConstructor<TypeId_SIntPtr, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_SIntPtr>::value);

/*  defineConstant("minVal", ConstantInteger::getSigned(llvm::APInt::getSignedMinValue(64), this));
  defineConstant("maxVal", ConstantInteger::getSigned(llvm::APInt::getSignedMaxValue(64), this));*/

  PrimitiveToString<TypeId_SIntPtr>::value.init();
}

template<> uint32_t IntPtrType::numBits() const {
  return 64;
}

template<> ConversionRank IntPtrType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * IntPtrType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: UByte

template<> TypeDefn UByteType::typedefn(&Builtins::module, "ubyte", &UByteType::instance);
template<> TypeIdSet UByteType::MORE_GENERAL =
TypeIdSet::of(TypeId_UInt16, TypeId_UInt32, TypeId_UInt64);
template<> TypeIdSet UByteType::INCLUDES = TypeIdSet::noneOf();

template<> void UByteType::init() {
  irType_ = llvm::Type::getInt8Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt8, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_UInt8>::value);

  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(8), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(8), this));

  PrimitiveToString<TypeId_UInt8>::value.init();
}

template<> uint32_t UByteType::numBits() const {
  return 8;
}

template<> ConversionRank
UByteType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * UByteType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: UShort

template<> TypeDefn UShortType::typedefn(&Builtins::module, "ushort", &UShortType::instance);
template<> TypeIdSet UShortType::MORE_GENERAL = TypeIdSet::of(TypeId_UInt32, TypeId_UInt64);
template<> TypeIdSet UShortType::INCLUDES = TypeIdSet::of(TypeId_UInt8);

template<> void UShortType::init() {
  irType_ = llvm::Type::getInt16Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt16, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_UInt16>::value);

  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(16), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(16), this));

  PrimitiveToString<TypeId_UInt16>::value.init();
}

template<> uint32_t UShortType::numBits() const {
  return 16;
}

template<> ConversionRank
UShortType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * UShortType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: UInt

template<> TypeDefn UIntType::typedefn(&Builtins::module, "uint", &UIntType::instance);
template<> TypeIdSet UIntType::MORE_GENERAL = TypeIdSet::of(TypeId_UInt64);
template<> TypeIdSet UIntType::INCLUDES = TypeIdSet::of(TypeId_UInt8, TypeId_UInt16);

template<> void UIntType::init() {
  irType_ = llvm::Type::getInt32Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt32, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_UInt32>::value);

  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(32), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(32), this));

  PrimitiveToString<TypeId_UInt32>::value.init();
}

template<> uint32_t UIntType::numBits() const {
  return 32;
}

template<> ConversionRank
UIntType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * UIntType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: ULong

template<> TypeDefn ULongType::typedefn(&Builtins::module, "ulong", &ULongType::instance);
template<> TypeIdSet ULongType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet ULongType::INCLUDES =
TypeIdSet::of(TypeId_UInt8, TypeId_UInt16, TypeId_UInt32);

template<> void ULongType::init() {
  irType_ = llvm::Type::getInt64Ty(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_UInt64>::value);

  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(64), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(64), this));

  PrimitiveToString<TypeId_UInt64>::value.init();
}

template<> uint32_t ULongType::numBits() const {
  return 64;
}

template<> ConversionRank
ULongType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * ULongType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: UIntPtr

template<> TypeDefn UIntPtrType::typedefn(&Builtins::module, "uintptr", &UIntPtrType::instance);
template<> TypeIdSet UIntPtrType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet UIntPtrType::INCLUDES = TypeIdSet::noneOf();
//TypeIdSet::of(TypeId_UInt8, TypeId_UInt16, TypeId_UInt32);

template<> void UIntPtrType::init() {
  irType_ = llvm::Type::getInt8PtrTy(llvm::getGlobalContext());
  /*addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_Char>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt8>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt16>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt32>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_SInt64>::value);
  addMember(&PrimitiveConstructor<TypeId_UInt64, TypeId_UInt64>::value); */
  addMember(&PrimitiveConstructor<TypeId_UIntPtr, TypeId_UnsizedInt>::value);
  addMember(&PrimitiveToString<TypeId_UIntPtr>::value);

/*  defineConstant("minVal", ConstantInteger::getUnsigned(llvm::APInt::getMinValue(64), this));
  defineConstant("maxVal", ConstantInteger::getUnsigned(llvm::APInt::getMaxValue(64), this));*/

  PrimitiveToString<TypeId_UIntPtr>::value.init();
}

template<> uint32_t UIntPtrType::numBits() const {
  return 64;
}

template<> ConversionRank
UIntPtrType::convertImpl(const Conversion & cn) const {
  return convertToInteger(cn);
}

template<> Expr * UIntPtrType::nullInitValue() const {
  return new ConstantInteger(SourceLocation(), &instance,
      cast<llvm::ConstantInt>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Float

template<> TypeDefn FloatType::typedefn(&Builtins::module, "float", &FloatType::instance);
template<> TypeIdSet FloatType::MORE_GENERAL = TypeIdSet::of(TypeId_Double);
template<> TypeIdSet FloatType::INCLUDES = TypeIdSet::noneOf();

template<> void FloatType::init() {
  irType_ = llvm::Type::getFloatTy(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_Float, TypeId_Float>::value);
  addMember(&PrimitiveConstructor<TypeId_Float, TypeId_Double>::value);
  addMember(&PrimitiveToString<TypeId_Float>::value);

  PrimitiveToString<TypeId_Float>::value.init();
}

template<> uint32_t FloatType::numBits() const {
  return 32;
}

template<> ConversionRank FloatType::convertImpl(const Conversion & cn) const {
  return convertToFloat(cn);
}

template<> Expr * FloatType::nullInitValue() const {
  return new ConstantFloat(SourceLocation(), &instance,
      cast<llvm::ConstantFP>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Double

template<> TypeDefn DoubleType::typedefn(&Builtins::module, "double", &DoubleType::instance);
template<> TypeIdSet DoubleType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet DoubleType::INCLUDES = TypeIdSet::of(TypeId_Float);

template<> void DoubleType::init() {
  irType_ = llvm::Type::getDoubleTy(llvm::getGlobalContext());
  addMember(&PrimitiveConstructor<TypeId_Double, TypeId_Double>::value);
  addMember(&PrimitiveToString<TypeId_Double>::value);

  PrimitiveToString<TypeId_Double>::value.init();
}

template<> uint32_t DoubleType::numBits() const {
  return 64;
}

template<>
ConversionRank DoubleType::convertImpl(const Conversion & cn) const {
  return convertToFloat(cn);
}

template<> Expr * DoubleType::nullInitValue() const {
  return new ConstantFloat(SourceLocation(), &instance,
      cast<llvm::ConstantFP>(llvm::Constant::getNullValue(irType_)));
}

/// -------------------------------------------------------------------
/// Primitive type: Null

template<> TypeDefn NullType::typedefn(&Builtins::module, "Null", &NullType::instance);
template<> TypeIdSet NullType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet NullType::INCLUDES = TypeIdSet::noneOf();

template<> void NullType::init() {
  irType_ = llvm::PointerType::get(llvm::StructType::get(llvm::getGlobalContext(), false), 0);
}

template<> uint32_t NullType::numBits() const {
  return 0;
}

template<> ConversionRank
NullType::convertImpl(const Conversion & cn) const {
  if (cn.getFromType() != this) {
    return Incompatible;
  }

  if (cn.resultValue) {
    *cn.resultValue = cn.fromValue;
  }

  return IdenticalTypes;
}

template<> Expr * NullType::nullInitValue() const {
  return new ConstantNull(SourceLocation());
}

/// -------------------------------------------------------------------
/// Primitive type: Any

template<> TypeDefn AnyType::typedefn(&Builtins::module, "__Any", &AnyType::instance);
template<> TypeIdSet AnyType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet AnyType::INCLUDES = TypeIdSet::noneOf();

template<> void AnyType::init() {
  irType_ = llvm::OpaqueType::get(llvm::getGlobalContext());
}

template<> uint32_t AnyType::numBits() const {
  return 0;
}

template<> ConversionRank
AnyType::convertImpl(const Conversion & cn) const {
  if (cn.resultValue) {
    *cn.resultValue = cn.fromValue;
  }

  return NonPreferred;
}

template<> Expr * AnyType::nullInitValue() const {
  DFAIL("IllegalState");
}

/// -------------------------------------------------------------------
/// Primitive type: UnsizedInt

template<> TypeDefn UnsizedIntType::typedefn(&Builtins::module, "#constant_int",
    &UnsizedIntType::instance);
template<> TypeIdSet UnsizedIntType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet UnsizedIntType::INCLUDES = TypeIdSet::of(
    TypeId_SInt8, TypeId_SInt16, TypeId_SInt32, TypeId_SInt64,
    TypeId_UInt8, TypeId_UInt16, TypeId_UInt32, TypeId_UInt64);

template<> void UnsizedIntType::init() {
  // irType_ = NULL
}

template<> uint32_t UnsizedIntType::numBits() const {
  return 0;
}

template<> ConversionRank
UnsizedIntType::convertImpl(const Conversion & cn) const {
  if (cn.getFromType() != this) {
    return Incompatible;
  }

  if (cn.resultValue) {
    *cn.resultValue = cn.fromValue;
  }

  return IdenticalTypes;
}

template<> Expr * UnsizedIntType::nullInitValue() const {
  return NULL;
}

PrimitiveType * PrimitiveType::fitIntegerType(size_t nBits, bool isUnsigned) {
  if (isUnsigned) {
    if (nBits <= 8) {
      return &UByteType::instance;
    } else if (nBits <= 16) {
      return &UShortType::instance;
    } else if (nBits <= 32) {
      return &UIntType::instance;
    } else if (nBits <= 64) {
      return &ULongType::instance;
    }
  } else {
    if (nBits <= 8) {
      return &ByteType::instance;
    } else if (nBits <= 16) {
      return &ShortType::instance;
    } else if (nBits <= 32) {
      return &IntType::instance;
    } else if (nBits <= 64) {
      return &LongType::instance;
    }
  }

  diag.error() << "Integer value requires " << nBits << " bits, too large.";
  return &LongType::instance;
}

/// -------------------------------------------------------------------
/// Primitive type: Bad

template<> TypeDefn BadType::typedefn(&Builtins::module, "<bad>", &BadType::instance);
template<> TypeIdSet BadType::MORE_GENERAL = TypeIdSet::noneOf();
template<> TypeIdSet BadType::INCLUDES = TypeIdSet::noneOf();

template<> void BadType::init() {
  irType_ = llvm::StructType::get(llvm::getGlobalContext(), false);
}

template<> uint32_t BadType::numBits() const {
  return 0;
}

template<> ConversionRank
BadType::convertImpl(const Conversion & cn) const {
  return Incompatible;
}

template<> Expr * BadType::nullInitValue() const {
  return NULL;
}

}
