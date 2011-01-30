/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_REFLECTOR_H
#define TART_GEN_REFLECTOR_H

#include "tart/Common/Agenda.h"
#include "tart/Common/SourceLocation.h"

#ifndef TART_META_NAMETABLE_H
  #include "tart/Meta/NameTable.h"
#endif

#include "tart/CFG/CFG.h"

#include "llvm/Support/IRBuilder.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/DenseSet.h"
#include "iostream"

namespace tart {

class Module;
class Type;
class Defn;
class ValueDefn;
class VariableDefn;
class FunctionDefn;
class NamespaceDefn;
class PropertyDefn;
class CompositeType;
class CodeGenerator;
class EnumType;
class PrimitiveType;
class FunctionType;
class IterableScope;
class TypeDefn;
class TupleType;

typedef std::vector<llvm::Constant *> ConstantList;
typedef llvm::StringMap<llvm::GlobalVariable *> GlobalVarMap;

/// -------------------------------------------------------------------
/// Class to handle generation of reflection data.
class Reflector {
public:
  // Keep these enums in sync with Member.tart
  enum Visibility {
    PUBLIC,
    PROTECTED,
    PRIVATE,
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
    NATIVE_ARRAY,
    //SingleValue
  };

  Reflector(CodeGenerator & cg);
  ~Reflector();

  /** Whether reflection is enabled. */
  bool enabled() const { return enabled_; }
  void setEnabled(bool enabled) { enabled_ = enabled; }

  /** Given the name of a symbol, return a unique interned string for that name. Identical
      strings will be combined across module boundaries. */
  llvm::Constant * internSymbol(const llvm::StringRef &Key);

  /** Generate a pointer to a module's reflection info. */
  llvm::GlobalVariable * getModulePtr(Module * module);

  llvm::GlobalVariable * getNameTablePtr(Module * module);

  /** Generate a pointer to the package reflection info. */
  llvm::GlobalVariable * getPackagePtr(Module * module);

  /** Generate a pointer to a reflected method. */
  llvm::GlobalVariable * getMethodPtr(const FunctionDefn * fn);

  /** Generate a pointer to a reflected property. */
  llvm::GlobalVariable * getPropertyPtr(const PropertyDefn * prop);

  /** Generate a pointer to a reflected field. */
  llvm::GlobalVariable * getFieldPtr(const VariableDefn * field);

  /** Generate reflection information for a module. */
  void emitModule(Module * module);

  /** Generate the name table which contains tables of names used by the module
      and the definitions within it. */
  void emitNameTable(Module * module);

  /** Add a definition to the list of reflected members. */
  void getRefs(const Defn * def);

  /** Return a reference to the reflected type object for the specified type. */
  llvm::Constant * getTypePtr(const Type * type);
  llvm::GlobalVariable * getCompositeTypePtr(const CompositeType * type);
  llvm::GlobalVariable * getEnumTypePtr(const EnumType * type);
  llvm::GlobalVariable * getDerivedTypePtr(const Type * type);
  llvm::GlobalVariable * getFunctionTypePtr(const FunctionType * type);

  /** Write out reflection information for a member. */
  void emitDefn(const Defn * def);
  void emitMethod(const FunctionDefn * fn);
  void emitProperty(const PropertyDefn * prop);
  void emitField(const VariableDefn * field);
  llvm::Constant * emitMember(const ValueDefn * member, const CompositeType * memberType,
      llvm::StringRef name);

  /** Write out reflection information for a type. */
  void emitType(const Type * type);
  void emitCompositeType(const CompositeType * type);
  void emitEnumType(const EnumType * type);
  void emitDerivedType(const Type * type);
  void emitFunctionType(const FunctionType * type);

  /** Write out the array of member types defined within the given scope. */
  llvm::Constant * emitMemberTypes(const IterableScope * scope);

  /** Write out the array of reflected Attribute objects defined within the given scope. */
  llvm::Constant * emitAttributeList(const ExprList & attrs, llvm::StringRef name);

  /** Write out the array of reflected Method objects defined within the given scope.
      If 'ctors' is true, include only constructors, otherwise only include non-constructors. */
  llvm::Constant * emitMethodList(const IterableScope * scope, bool ctors, llvm::StringRef name);

  /** Write out the array of reflected Property objects within the given scope. */
  llvm::Constant * emitPropertList(const IterableScope * scope, llvm::StringRef name);

  /** Write out the array of reflected Field objects within the given scope. */
  llvm::Constant * emitFieldList(const IterableScope * scope, llvm::StringRef name);

  /** Generate a TypeList from the given list of types. */
  llvm::Constant * emitTypeList(const ConstTypeList & types);

  /** Generate a StaticList from the given list of constant elements. */
  llvm::Constant * emitStaticList(const ConstantList & elements,
      llvm::StringRef namePrefix, llvm::StringRef name,
      const CompositeType * listType, const CompositeType * elementType);

  /** Generate an array containing reflection data supplied by the specified array. */
  llvm::Constant * emitArray(
      const std::string & baseName, const VariableDefn * var, const ConstantList & values);

private:

  NameTable::Name * addQualifiedName(llvm::StringRef name);
  NameTable::Name * addName(const llvm::StringRef name);

  Module * module();

  llvm::Constant * getRetainedAttr(const Expr * attrExpr);
  bool isExport(const Defn * de);
  int typeKind(Type::TypeClass cls);

  CodeGenerator & cg_;
  bool enabled_;
  llvm::LLVMContext & context_;
  llvm::IRBuilder<true> builder_;    // LLVM builder
  llvm::Module * irModule_;
  llvm::GlobalVariable * nameTableVar_;

  GlobalVarMap globals_;

  // Set of types exported by this module.
  Agenda<const Defn> defnExports_;
  Agenda<const Type> typeExports_;
  bool outputPhase_;
};

}

#endif
