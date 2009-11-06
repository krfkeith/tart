/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_REFLECTOR_H
#define TART_GEN_REFLECTOR_H

#include "tart/Common/SourceLocation.h"

#include "llvm/Support/IRBuilder.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/SetVector.h"
#include "iostream"

namespace tart {

class Module;
class Type;
class Defn;
class ValueDefn;
class VariableDefn;
class FunctionDefn;
class CompositeType;
class CodeGenerator;
class EnumType;
class PrimitiveType;
class FunctionType;
class IterableScope;
class TypeDefn;
class TypeRef;
class TypeVector;

typedef std::vector<llvm::Constant *> ConstantList;
typedef llvm::StringMap<llvm::Constant *> SymbolNameMap;
typedef llvm::StringMap<llvm::GlobalVariable *> GlobalVarMap;
typedef llvm::SetVector<Defn *> DefnSet;

/// -------------------------------------------------------------------
/// Represents all of the reflected symbols within a single scope.

struct ReflectedMembers {
  ConstantList methods;
  ConstantList types;
};

/// -------------------------------------------------------------------
/// Class to handle generation of reflection data.
class Reflector {
public:
  // Keep these enums in sync with Member.tart
  enum Access {
    PUBLIC,
    PROTECTED,
    PRIVATE,
  };

  enum MemberKind {
    FIELD,
    PROPERTY,
    METHOD,
    CONSTRUCTOR,
  };

  enum Traits {
    FINAL     = (1 << 0),
    ABSTRACT  = (1 << 1),
    STATIC    = (1 << 2),
  };

  // Keep this enum in sync with Type.tart
  enum TypeKind {
    OPAQUE,
    PRIMITIVE,
    CLASS,
    STRUCT,
    INTERFACE,
    PROTOCOL,
    ENUM,
    FUNCTION,
    TUPLE,
    UNION,
    ADDRESS,
    POINTER,
    NATIVE_ARRAY,
    //SingleValue
  };

  // Keep this enum in sync with SimpleType.tart
  enum SubtypeId {
    NONE = 0,
    VOID,
    BOOL,
    CHAR,
    BYTE,
    SHORT,
    INT,
    LONG,
    UBYTE,
    USHORT,
    UINT,
    ULONG,
    FLOAT,
    DOUBLE,
  };

  Reflector(CodeGenerator & cg);

  /** Whether reflection is enabled. */
  bool enabled() const { return enabled_; }
  void setEnabled(bool enabled) { enabled_ = enabled; }

  /** Given the name of a symbol, return a unique interned string for that name. Identical
      strings will be combined across module boundaries. */
  llvm::Constant * internSymbol(const llvm::StringRef &Key);

  /** Generate a pointer to a module's reflection info. */
  llvm::GlobalVariable * getModulePtr(Module * module);

  /** Generate a pointer to a type's reflection info. */
  llvm::GlobalVariable * getTypePtr(const Type * type);

  /** Generate reflection information for a module. */
  void emitModule(Module * module);

  /** Generate reflection information for a type definition in this module. */
  llvm::GlobalVariable * emitTypeDefn(const TypeDefn * td);

  /** Generate reflection information for a method. */
  llvm::Constant * emitMethod(const FunctionDefn * func);

  /** Generate reflection information for a Member struct. */
  llvm::Constant * emitMember(const CompositeType * structType, const ValueDefn * def);

  /** Generate an array containing reflection data supplied by the specified array. */
  llvm::Constant * emitArray(
      const std::string & baseName, const VariableDefn * var, const ConstantList & values);

  /** Generate a typeRef struct for the given type reference. */
  llvm::Constant * emitTypeReference(const TypeRef & type);

  /** Return the LLVM type of the reflection infor for this type. */
  const llvm::Type * Reflector::reflectedTypeOf(const Type * type);

  /** Generate a Type object and return a pointer to it. */
  llvm::Constant * emitType(const Type * type);
  llvm::Constant * emitPrimitiveType(const PrimitiveType * type);
  llvm::Constant * emitCompositeType(const CompositeType * type);
  llvm::Constant * emitEnumType(const EnumType * type);
  llvm::Constant * emitFunctionType(const FunctionType * type);
  llvm::Constant * emitDerivedType(const Type * type);
  llvm::Constant * emitOpaqueType(const Type * type);
  llvm::Constant * emitSimpleType(const Type * reflectType, const Type * type);
  llvm::Constant * emitTypeBase(const Type * reflectType, TypeKind kind);
  llvm::Constant * emitTypeVector(TypeVector * types);

  /** Return the type of the 'invoke' function for a function type. */
  llvm::FunctionType * getInvokeFnType();

private:
  bool visitMembers(ReflectedMembers & rs, const IterableScope * scope);
  bool visitMember(ReflectedMembers & rm, const Defn * member);

  Access memberAccess(const Defn * member);
  MemberKind memberKind(const Defn * member);
  Traits memberTraits(const Defn * member);

  CodeGenerator & cg_;
  bool enabled_;
  llvm::LLVMContext & context_;
  llvm::IRBuilder<true> builder_;    // LLVM builder
  llvm::Module * irModule_;
  llvm::GlobalVariable * moduleTable_;

  SymbolNameMap symbols_;
  GlobalVarMap globals_;
  DefnSet synthetics_;
  size_t syntheticIndex_;
};

}

#endif
