/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"

#include "llvm/Function.h"
#include "llvm/Module.h"

namespace tart {

using namespace llvm;
#if 0
DIType CodeGenerator::genTypeDebugInfo(Type * type) {
  DITypeMap::const_iterator it = dbgTypeMap_.find(type);
  if (it != dbgTypeMap_.end()) {
    return it->second();
  }

  DIType result;
  switch (type->typeClass()) {
    case Type::Primitive:
      result = genPrimitiveTypeDebugInfo(static_cast<PrimitiveType *>(type));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
      result = genCompositeTypeDebugInfo(static_cast<CompositeType *>(type));

    case Type::Enum:
      result = genEnumTypeDebugInfo(static_cast<EnumType *>(type));

    case Type::NativeArray:
      result = genNativeArrayTypeDebugInfo(static_cast<NativeArrayType *>(type));

    case Type::NativePointer:
      result = genNativePointerTypeDebugInfo(static_cast<NativePointerType *>(type));

    case Type::Union:
      result = genUnionTypeDebugInfo(static_cast<UnionType *>(type));

    case Type::Function:
      result = genFunctionTypeDebugInfo(static_cast<FunctionType *>(type));

    case Type::Alias: {
      TypeAlias * alias = static_cast<TypeAlias *>(type);
      return genTypeDebugInf(alias->value());
    }

    default:
      diag.debug() << type;
      DFAIL("Invalid type defn");
      break;
  }

  dgbTypeMap_[type] = result;
  return result;
}

DIType CodeGenerator::genPrimitiveTypeDebugInfo(PrimitiveType * type) {
  /// CreateBasicType - Create a basic type like int, float, etc.
  return CreateBasicType(dbgCompileUnit_, type->name(), NULL, 0,
                              uint64_t SizeInBits, uint64_t AlignInBits,
                              0, unsigned Flags,
                              unsigned Encoding);

  DW_ATE_address       = 1
  DW_ATE_boolean       = 2
  DW_ATE_float         = 4
  DW_ATE_signed        = 5
  DW_ATE_signed_char   = 6
  DW_ATE_unsigned      = 7
  DW_ATE_unsigned_char = 8

}

DIType CodeGenerator::genCompositeTypeDebugInfo(CompositeType * type) {
  /// CreateCompositeType - Create a composite type like array, struct, etc.
  DICompositeType CreateCompositeType(unsigned Tag, DIDescriptor Context,
                                      const std::string &Name,
                                      DICompileUnit CompileUnit,
                                      unsigned LineNumber,
                                      uint64_t SizeInBits,
                                      uint64_t AlignInBits,
                                      uint64_t OffsetInBits, unsigned Flags,
                                      DIType DerivedFrom,
                                      DIArray Elements,
                                      unsigned RunTimeLang = 0);

}

DIType CodeGenerator::genEnumTypeDebugInfo(EnumType * type) {
  /// CreateBasicType - Create a basic type like int, float, etc.
  DIBasicType CreateBasicType(DIDescriptor Context, const std::string &Name,
                              DICompileUnit CompileUnit, unsigned LineNumber,
                              uint64_t SizeInBits, uint64_t AlignInBits,
                              uint64_t OffsetInBits, unsigned Flags,
                              unsigned Encoding);
  /// CreateEnumerator - Create a single enumerator value.
  DIEnumerator CreateEnumerator(const std::string &Name, uint64_t Val);

}

DIType CodeGenerator::genNativeArrayTypeDebugInfo(NativeArrayType * type) {
  /// CreateDerivedType - Create a derived type like const qualified type,
  /// pointer, typedef, etc.
  DIDerivedType CreateDerivedType(unsigned Tag, DIDescriptor Context,
                                  const std::string &Name,
                                  DICompileUnit CompileUnit,
                                  unsigned LineNumber,
                                  uint64_t SizeInBits, uint64_t AlignInBits,
                                  uint64_t OffsetInBits, unsigned Flags,
                                  DIType DerivedFrom);
}

DIType CodeGenerator::genNativePointerTypeDebugInfo(NativePointerType * type) {
  /// CreateDerivedType - Create a derived type like const qualified type,
  /// pointer, typedef, etc.
  DIDerivedType CreateDerivedType(unsigned Tag, DIDescriptor Context,
                                  const std::string &Name,
                                  DICompileUnit CompileUnit,
                                  unsigned LineNumber,
                                  uint64_t SizeInBits, uint64_t AlignInBits,
                                  uint64_t OffsetInBits, unsigned Flags,
                                  DIType DerivedFrom);
}

DIType CodeGenerator::genUnionTypeDebugInfo(UnionType * type) {
  /// CreateCompositeType - Create a composite type like array, struct, etc.
  DICompositeType CreateCompositeType(unsigned Tag, DIDescriptor Context,
                                      const std::string &Name,
                                      DICompileUnit CompileUnit,
                                      unsigned LineNumber,
                                      uint64_t SizeInBits,
                                      uint64_t AlignInBits,
                                      uint64_t OffsetInBits, unsigned Flags,
                                      DIType DerivedFrom,
                                      DIArray Elements,
                                      unsigned RunTimeLang = 0);
}

DIType CodeGenerator::genFunctionTypeDebugInfo(FunctionType * type) {
  DFAIL("Implement");
}
#endif

} // namespace tart
