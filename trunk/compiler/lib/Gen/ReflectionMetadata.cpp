/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/NamespaceDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/TypeOrdering.h"

#include "tart/Meta/Tags.h"
#include "tart/Meta/VarInt.h"

#include "tart/Gen/ReflectionMetadata.h"

namespace tart {

namespace {

struct TypeOrder {
  bool operator()(
      const ReflectionMetadata::TypeArrayElement & t0,
      const ReflectionMetadata::TypeArrayElement & t1) {
    if (t0.second.useCount > t1.second.useCount) return true;
    if (t0.second.useCount < t1.second.useCount) return false;
    return LexicalTypeOrdering::compare(t0.first, t1.first) > 0;
  }
};

}

/// -------------------------------------------------------------------
/// ReflectionMetadata

void ReflectionMetadata::addTypeRef(const Type * type) {
  DASSERT(type != NULL);
  TypeMap::iterator it = types_.find(type);
  if (it != types_.end()) {
    it->second.useCount++;
    return;
  }

  switch (type->typeClass()) {
    case Type::Primitive:
      return;

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol: {
      const CompositeType * ctype = static_cast<const CompositeType *>(type);
      TypeDefn * classDefn = ctype->typeDefn();
      types_[ctype] = TagInfo(1);
      mmd_.defnsToExport().append(classDefn);
      break;
    }

    case Type::Enum: {
      const EnumType * etype = static_cast<const EnumType *>(type);
      TypeDefn * enumDefn = etype->typeDefn();
      mmd_.defnsToExport().append(enumDefn);
      types_[etype] = TagInfo(1);
      break;
    }

    case Type::Function: {
      const FunctionType * ftype = static_cast<const FunctionType *>(type);
      types_[ftype] = TagInfo(1);
      addTypeRef(ftype->returnType());
      if (ftype->selfParam()) {
        addTypeRef(ftype->selfParam()->type());
      }
      addTypeRef(ftype->paramTypes());
      break;
    }

    case Type::Union: {
      const UnionType * utype = static_cast<const UnionType *>(type);
      types_[utype] = TagInfo(1);
      for (ConstTypeList::const_iterator it = utype->members().begin();
          it != utype->members().end(); ++it) {
        addTypeRef(*it);
      }
      break;
    }

    case Type::Tuple: {
      const TupleType * ttype = static_cast<const TupleType *>(type);
      types_[ttype] = TagInfo(1);
      for (ConstTypeList::const_iterator it = ttype->members().begin();
          it != ttype->members().end(); ++it) {
        addTypeRef(*it);
      }
      break;
    }

    case Type::NAddress: {
      const AddressType * atype = static_cast<const AddressType *>(type);
      types_[atype] = TagInfo(1);
      addTypeRef(atype->typeParam(0));
      break;
    }

    case Type::NArray: {
      const NativeArrayType * natype = static_cast<const NativeArrayType *>(type);
      types_[natype] = TagInfo(1);
      addTypeRef(natype->typeParam(0));
      break;
    }

    case Type::FlexibleArray: {
      const FlexibleArrayType * natype = static_cast<const FlexibleArrayType *>(type);
      types_[natype] = TagInfo(1);
      addTypeRef(natype->typeParam(0));
      break;
    }

    case Type::TypeLiteral: {
      const TypeLiteralType * ttype = static_cast<const TypeLiteralType *>(type);
//      return visitTypeLiteralType(static_cast<const TypeLiteralType *>(in));
      break;
    }

    case Type::Unit: {
      const UnitType * utype = static_cast<const UnitType *>(type);
      types_[utype] = TagInfo(1);
//      return visitUnitType(static_cast<const UnitType *>(in));
      break;
    }

    case Type::Alias:
      DFAIL("Not handled");
      break;

    case Type::TypeVar: {
      const TypeVariable * typeVar = static_cast<const TypeVariable *>(type);
      types_[typeVar] = TagInfo(1);
      mmd_.names().addName(typeVar->name())->use();
      break;
    }

//    case Type::Binding:
//      return visitTypeBinding(static_cast<const TypeBinding *>(in));

    default:
      diag.fatal() << "Type class not handled: " << type->typeClass();
  }
}

size_t ReflectionMetadata::addRetainedAttribute(llvm::Constant * attribute) {
  size_t index = retainedAttrs_.size();
  retainedAttrs_.push_back(attribute);
  return index;
}

void ReflectionMetadata::assignIndices() {
  for (TypeMap::iterator it = types_.begin(); it != types_.end(); ++it) {
    // Only insert types into the table if there's more than one; Otherwise
    // we'll just put the type definition inline.
    if (isa<CompositeType>(it->first)) {
      compositeTypeRefs_.push_back(*it);
    } else if (isa<EnumType>(it->first)) {
      enumTypeRefs_.push_back(*it);
    } else if (it->second.useCount > 1) {
      derivedTypeRefs_.push_back(*it);
    } else {
      it->second.index = -1;
    }
  }

  std::sort(derivedTypeRefs_.begin(), derivedTypeRefs_.end(), TypeOrder());
  for (unsigned i = 0; i < derivedTypeRefs_.size(); ++i) {
    derivedTypeRefs_[i].second.index = i;
    types_[derivedTypeRefs_[i].first].index = i;
  }

  std::sort(compositeTypeRefs_.begin(), compositeTypeRefs_.end(), TypeOrder());
  for (unsigned i = 0; i < compositeTypeRefs_.size(); ++i) {
    compositeTypeRefs_[i].second.index = i;
    types_[compositeTypeRefs_[i].first].index = i;
  }

  std::sort(enumTypeRefs_.begin(), enumTypeRefs_.end(), TypeOrder());
  for (unsigned i = 0; i < enumTypeRefs_.size(); ++i) {
    enumTypeRefs_[i].second.index = i;
    types_[enumTypeRefs_[i].first].index = i;
  }
}


void ReflectionMetadata::dump() const {
  diag.debug() << "Reflection metadata for " << reflectedDefn_->linkageName();
  diag.indent();
  if (!derivedTypeRefs_.empty()) {
    diag.debug() << derivedTypeRefs_.size() << " unique derived types added";
    diag.indent();
    for (TypeArray::const_iterator it = derivedTypeRefs_.begin(); it != derivedTypeRefs_.end();
        ++it) {
      diag.debug() << Format_Verbose << it->second.index << " " << it->first <<
          " (" << it->second.useCount << ")";
    }
    diag.unindent();
  }

  if (!compositeTypeRefs_.empty()) {
    diag.debug() << compositeTypeRefs_.size() << " unique compound types added";
    diag.indent();
    for (TypeArray::const_iterator it = compositeTypeRefs_.begin(); it != compositeTypeRefs_.end();
        ++it) {
      diag.debug() << Format_Verbose << it->second.index << " " << it->first <<
          " (" << it->second.useCount << ")";
    }
    diag.unindent();
  }

  if (!enumTypeRefs_.empty()) {
    diag.debug() << enumTypeRefs_.size() << " unique enum types added";
    diag.indent();
    for (TypeArray::const_iterator it = enumTypeRefs_.begin(); it != enumTypeRefs_.end(); ++it) {
      diag.debug() << Format_Verbose << it->second.index << " " << it->first <<
          " (" << it->second.useCount << ")";
    }
    diag.unindent();
  }
  diag.unindent();
}

void ReflectionMetadata::encodeTypesTable(llvm::raw_ostream & out) {
  if (!derivedTypeRefs_.empty()) {
    out << VarInt(derivedTypeRefs_.size());
    for (TypeArray::const_iterator it = derivedTypeRefs_.begin();
        it != derivedTypeRefs_.end(); ++it) {
      encodeType(it->first, out);
    }
  }
  out.flush();
}

void ReflectionMetadata::encodeType(const Type * type, llvm::raw_ostream & out) {
  DASSERT(type != NULL);

  switch (type->typeClass()) {
    case Type::Primitive: {
      const PrimitiveType * ptype = static_cast<const PrimitiveType *>(type);
      switch (ptype->typeId()) {
        case TypeId_Void: out << char(TAG_TYPE_VOID); break;
        case TypeId_Bool: out << char(TAG_TYPE_BOOL); break;
        case TypeId_Char: out << char(TAG_TYPE_CHAR); break;
        case TypeId_SInt8: out << char(TAG_TYPE_INT8); break;
        case TypeId_SInt16: out << char(TAG_TYPE_INT16); break;
        case TypeId_SInt32: out << char(TAG_TYPE_INT32); break;
        case TypeId_SInt64: out << char(TAG_TYPE_INT64); break;
        case TypeId_UInt8: out << char(TAG_TYPE_UINT8); break;
        case TypeId_UInt16: out << char(TAG_TYPE_UINT16); break;
        case TypeId_UInt32: out << char(TAG_TYPE_UINT32); break;
        case TypeId_UInt64: out << char(TAG_TYPE_UINT64); break;
        case TypeId_Float: out << char(TAG_TYPE_FLOAT); break;
        case TypeId_Double: out << char(TAG_TYPE_DOUBLE); break;
        //case TypeId_LongDouble: out << char(TAG_TYPE_LONG); break;
        //case TypeId_UnsizedInt: out << char(TAG_TYPE_VOID); break;
        case TypeId_Null: out << char(TAG_TYPE_NULL); break;
        default:
          DFAIL("Type tag not implemented");
          break;
      }
      break;
    }

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol: {
      diag.debug() << "Composite type not indexed: " << type;
      DFAIL("Internal error");
      break;
    }

    case Type::Enum: {
      diag.debug() << "Enum type not indexed: " << type;
      DFAIL("Internal error");
      break;
    }

    case Type::Function: {
      const FunctionType * ftype = static_cast<const FunctionType *>(type);
      out << char(ftype->isStatic() ? TAG_TYPE_FUNCTION_STATIC : TAG_TYPE_FUNCTION);

      // For now, we only support reflected invocation of global, class, or interface methods.
      const Type * selfType = ftype->selfParam() != NULL ? ftype->selfParam()->type() : NULL;
      int invokeIndex = 0;
      if (selfType == NULL ||
              selfType->typeClass() == Type::Class ||
              selfType->typeClass() == Type::Interface) {
        TypeMap::iterator it = mmd_.invokeMap().find(ftype);
        if (it != mmd_.invokeMap().end()) {
          invokeIndex = it->second.index;
        }
      }

      out << VarInt(invokeIndex);
      encodeTypeRef(ftype->returnType(), out);
      encodeTypeRef(ftype->paramTypes(), out);
      break;
    }

    case Type::Union: {
      const UnionType * utype = static_cast<const UnionType *>(type);
      out << char(TAG_TYPE_UNION) << VarInt(utype->members().size());
      for (ConstTypeList::const_iterator it = utype->members().begin();
          it != utype->members().end(); ++it) {
        encodeTypeRef(*it, out);
      }
      break;
    }

    case Type::Tuple: {
      const TupleType * ttype = static_cast<const TupleType *>(type);
      out << char(TAG_TYPE_TUPLE) << VarInt(ttype->members().size());
      for (ConstTypeList::const_iterator it = ttype->members().begin();
          it != ttype->members().end(); ++it) {
        encodeTypeRef(*it, out);
      }
      break;
    }

    case Type::NAddress: {
      const AddressType * atype = static_cast<const AddressType *>(type);
      out << char(TAG_TYPE_NADDRESS);
      encodeTypeRef(atype->typeParam(0), out);
      break;
    }

    case Type::NArray: {
      const NativeArrayType * natype = static_cast<const NativeArrayType *>(type);
//      return visitNativeArrayType(static_cast<const NativeArrayType *>(in));
      DFAIL("Implement");
      break;
    }

    case Type::FlexibleArray: {
      const FlexibleArrayType * natype = static_cast<const FlexibleArrayType *>(type);
//      return visitFlexibleArrayType(static_cast<const FlexibleArrayType *>(in));
      DFAIL("Implement");
      break;
    }

    case Type::TypeLiteral: {
      const TypeLiteralType * ttype = static_cast<const TypeLiteralType *>(type);
      out << char(TAG_TYPE_TYPELITERAL);
      encodeType(ttype->typeParam(0), out);
      break;
    }

    case Type::Unit: {
      const UnitType * utype = static_cast<const UnitType *>(type);
//      return visitUnitType(static_cast<const UnitType *>(in));
      DFAIL("Implement");
      break;
    }

    case Type::Alias:
      DFAIL("Not handled");
      break;

    case Type::TypeVar: {
      const TypeVariable * typeVar = static_cast<const TypeVariable *>(type);
      // TODO - we really ought to record the variable index, not the name.
      out << char(TAG_TYPE_TYPEVAR);
//      return visitTypeVariable(static_cast<const TypeVariable *>(in));
      break;
    }

    default:
      diag.fatal() << "Type class not handled: " << type->typeClass();
      break;
  }
}

void ReflectionMetadata::encodeTypeRef(const Type * type, llvm::raw_ostream & out) {
  DASSERT(type != NULL);
  TypeMap::iterator it = types_.find(type);
  if (isa<CompositeType>(type)) {
    DASSERT_OBJ(it != types_.end(), type);
    DASSERT_OBJ(it->first == type, type);
    unsigned index = it->second.index;

    if (index < 64) {
      // Write out the composite type index using compact notation
      out << char(TAG_TYPE_COMPOSITE_IMM + index);
    } else {
      // Write out the composite type index using extended notation.
      out << char(TAG_TYPE_COMPOSITE) << VarInt(index);
    }
  } else if (isa<EnumType>(type)) {
    DASSERT_OBJ(it != types_.end(), type);
    unsigned index = it->second.index;

    if (index < 16) {
      // Write out the composite type index using compact notation
      out << char(TAG_TYPE_ENUM_IMM + index);
    } else {
      // Write out the composite type index using extended notation.
      out << char(TAG_TYPE_ENUM) << VarInt(index);
    }
  } else {
    if (it != types_.end() && it->second.useCount > 1) {
      // Emit a reference to the type
      unsigned index = it->second.index;
      if (index < 128) {
        // Write out the derived type index using compact notation
        out << char(TAG_TYPE_DERIVED_IMM + index);
      } else {
        // Write out the derived type index using extended notation
        out << char(TAG_TYPE_DERIVED) << VarInt(index);
      }
    } else {
      // Write out the type definition inline.
      encodeType(type, out);
    }
  }
}

#if 0

#include "tart/AST/ASTDecl.h"
#include "tart/CFG/TypeOrdering.h"
#include "tart/CFG/VariableDefn.h"
#include "tart/Gen/ReflectionMetadata.h"
#include "tart/Meta/Tags.h"
#include "tart/Meta/VarInt.h"
#include <vector>

namespace tart {

using llvm::StringRef;

namespace {

/// -------------------------------------------------------------------
/// Comparator for names by use count.

struct TypeOrder {
  bool operator()(
      const ReflectionMetadata::TypeArrayElement & t0,
      const ReflectionMetadata::TypeArrayElement & t1) {
    if (t0.second.useCount > t1.second.useCount) return true;
    if (t0.second.useCount > t1.second.useCount) return false;
    return LexicalTypeOrdering::compare(t0.first, t1.first) > 0;
  }
};

}

// -------------------------------------------------------------------
// ReflectionMetadata

ReflectionMetadata::~ReflectionMetadata() {
}


void ReflectionMetadata::addASTDecl(const ASTDecl * ast) {
  switch (ast->nodeType()) {
    case ASTNode::Class:
    case ASTNode::Struct:
    case ASTNode::Interface:
    case ASTNode::Protocol: {
      names_.addName(ast->name())->use();
      const ASTTypeDecl * type = static_cast<const ASTTypeDecl *>(ast);
      for (ASTNodeList::const_iterator it = type->attributes().begin();
          it != type->attributes().end(); ++it) {
      }
      for (ASTNodeList::const_iterator it = type->bases().begin();
          it != type->bases().end(); ++it) {
      }
      for (ASTNodeList::const_iterator it = type->imports().begin();
          it != type->imports().end(); ++it) {
      }
      for (ASTDeclList::const_iterator it = type->members().begin();
          it != type->members().end(); ++it) {
        addASTDecl(*it);
      }

      break;
    }

    case ASTNode::Enum: {
      names_.addName(ast->name())->use();
      const ASTTypeDecl * type = static_cast<const ASTTypeDecl *>(ast);
      for (ASTNodeList::const_iterator it = type->attributes().begin();
          it != type->attributes().end(); ++it) {
      }
      for (ASTNodeList::const_iterator it = type->bases().begin();
          it != type->bases().end(); ++it) {
      }
      for (ASTDeclList::const_iterator it = type->members().begin();
          it != type->members().end(); ++it) {
        addASTDecl(*it);
      }
      break;
    }

    case ASTNode::Function:
    case ASTNode::Macro: {
      names_.addName(ast->name())->use();
      break;
    }

    case ASTNode::Let:
    case ASTNode::Var: {
      names_.addName(ast->name())->use();
      break;
    }

    case ASTNode::Prop:
    case ASTNode::Idx: {
      names_.addName(ast->name())->use();
      break;
    }

//    // Definition of all node types
//    NODE_TYPE(Invalid)
//
//    // Terminals
//    NODE_TYPE(Null)
//    NODE_TYPE(Id)
//    NODE_TYPE(Super)
//    NODE_TYPE(LitBool)
//    NODE_TYPE(LitChar)
//    NODE_TYPE(LitInt)
//    NODE_TYPE(LitFloat)
//    NODE_TYPE(LitDouble)
//    NODE_TYPE(LitString)
//    NODE_TYPE(BuiltIn)
//
//    // Other literals
//    NODE_TYPE(ArrayLiteral)
//
//    // Declarations
//    NODE_TYPE(Alias)
//    NODE_TYPE(VarList)
//    NODE_TYPE(Param)
//    NODE_TYPE(Template)
//    NODE_TYPE(TParam)
//    NODE_TYPE(TypeVar)
//    NODE_TYPE(Namespace)
//
//    // Type modifiers
//    NODE_TYPE(AnonFn)
//    NODE_TYPE(Array)
//    NODE_TYPE(Pointer)
//    NODE_TYPE(NativePointer)
//    NODE_TYPE(NativeArray)
//    NODE_TYPE(FlexibleArray)
//
//    // Binary operators
//    NODE_TYPE(Assign)
//    NODE_TYPE(AssignAdd)
//    NODE_TYPE(AssignSub)
//    NODE_TYPE(AssignMul)
//    NODE_TYPE(AssignDiv)
//    NODE_TYPE(AssignMod)
//    NODE_TYPE(AssignBitAnd)
//    NODE_TYPE(AssignBitOr)
//    NODE_TYPE(AssignBitXor)
//    NODE_TYPE(AssignRSh)
//    NODE_TYPE(AssignLSh)
//    NODE_TYPE(PostAssign)
//    NODE_TYPE(LogicalAnd)
//    NODE_TYPE(LogicalOr)
//    NODE_TYPE(Range)
//    NODE_TYPE(Tuple)
//    NODE_TYPE(AsType)
//    NODE_TYPE(Is)
//    NODE_TYPE(IsNot)
//    NODE_TYPE(In)
//    NODE_TYPE(NotIn)
//    NODE_TYPE(IsInstanceOf)
//    NODE_TYPE(TParamDefault)
//
//    // Unary operators
//    NODE_TYPE(Negate)
//    NODE_TYPE(LogicalNot)
//    NODE_TYPE(Complement)
//
//    // Evaluations and Invocations
//    NODE_TYPE(Call)         // Function call
//    NODE_TYPE(Specialize)   // Explicit template specialization
//    NODE_TYPE(Member)       // Reference to member (a.b)
//    NODE_TYPE(GetElement)   // Reference to array element (a[b])
//
//    // Argument keyword
//    NODE_TYPE(Keyword)
//
//    // Statements
//    NODE_TYPE(Block)
//    NODE_TYPE(Expression)
//    NODE_TYPE(If)
//    NODE_TYPE(While)
//    NODE_TYPE(DoWhile)
//    NODE_TYPE(For)
//    NODE_TYPE(ForEach)
//    NODE_TYPE(Throw)
//    NODE_TYPE(Try)
//    NODE_TYPE(Catch)
//    NODE_TYPE(Return)
//    NODE_TYPE(Yield)
//    NODE_TYPE(Break)
//    NODE_TYPE(Continue)
//    NODE_TYPE(LocalDecl)
//    NODE_TYPE(Intrinsic)
//    NODE_TYPE(Import)
//    NODE_TYPE(Switch)
//    NODE_TYPE(Case)
//    NODE_TYPE(Classify)
    default:
      diag.error() << ast << " AST Not handled";
      DFAIL("AST Not handled");
      break;
  }
}
#endif


} // namespace tart

