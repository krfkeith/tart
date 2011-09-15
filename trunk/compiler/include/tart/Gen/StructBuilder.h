/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_STRUCTBUILDER_H
#define TART_GEN_STRUCTBUILDER_H

#ifndef TART_TYPE_QUALIFIEDTYPE_H
#include "tart/Type/QualifiedType.h"
#endif

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

#ifndef LLVM_DERIVED_TYPES_H
#include "llvm/DerivedTypes.h"
#endif

#ifndef LLVM_CONSTANT_H
#include "llvm/Constant.h"
#endif

namespace tart {

class CodeGenerator;
class Type;
class CompositeType;
class VariableDefn;

/// -------------------------------------------------------------------
/// StructBuilder
class StructBuilder {
public:
  StructBuilder(CodeGenerator & gen);

  /** Create an object header for the given object type. */
  StructBuilder & createObjectHeader(const Type * type);

  /** Add a field containing a constant value. */
  StructBuilder & addField(llvm::Constant * value);

  /** Add a field containing a constant null pointer. */
  StructBuilder & addNullField(const Type * type);

  /** Add a field containing a constant null pointer. */
  StructBuilder & addNullField(QualifiedType type) {
    return addNullField(type.unqualified());
  }

  /** Add a field containing a constant null pointer. */
  StructBuilder & addNullField(VariableDefn * var);

  /** Add an integer field of the specified type. */
  StructBuilder & addIntegerField(const Type * type, int32_t value);

  /** Add an integer field with the same type as the variable 'var'. */
  StructBuilder & addIntegerField(VariableDefn * var, int32_t value);

  /** Add a String field. */
  StructBuilder & addStringField(llvm::StringRef strval);

  /** Add an pointer field with the same type as the variable 'var'. */
  StructBuilder & addPointerField(VariableDefn * var, llvm::Constant * value);

  /** Add a native array field consisting of elements of type 'elementType'. */
  StructBuilder & addArrayField(const Type * elementType, llvm::ArrayRef<llvm::Constant *> values);

  /** Add a native array whose type is derived from the type of 'arrayVar' and whose
      elements consist of 'values. */
  StructBuilder & addArrayField(
      const VariableDefn * arrayVar, llvm::ArrayRef<llvm::Constant *> values);

  /** Combine all of the members into a single structure field. */
  StructBuilder & combine(const Type * type);

  /** Build a constant struct from the member fields. */
  llvm::Constant * buildAnon() const;

  /** Build a constant struct from the member fields, and verify that it is the expected type. */
  llvm::Constant * build(llvm::Type * expectedType) const;

  /** Build a constant struct from the member fields, and verify that it is the expected type. */
  llvm::Constant * build(const CompositeType * expectedType) const;

  /** Given a named type with no body, set the type body to the types of the member fields,
      and then return a constant struct with that type. */
  llvm::Constant * buildBody(llvm::StructType * stype) const;

private:
  llvm::SmallVector<llvm::Constant *, 16> members_;
  CodeGenerator & gen_;
};

} // namespace tart

#endif // TART_GEN_STRUCTBUILDER_H
