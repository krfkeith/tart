/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_STRUCTBUILDER_H
#define TART_GEN_STRUCTBUILDER_H

#include <llvm/DerivedTypes.h>
#include <llvm/Constant.h>

namespace tart {

class CodeGenerator;
class Type;
class TypeRef;
class VariableDefn;

typedef std::vector<llvm::Constant *> ConstantList;

/// -------------------------------------------------------------------
/// StructBuilder
class StructBuilder {
public:
  StructBuilder(CodeGenerator & gen);

  /** Create an object header for the given object type. */
  StructBuilder & createObjectHeader(const Type * type);

  /** Add a field containing a constant value. */
  StructBuilder & addField(llvm::Constant * value);

  /** Add a field which contains a reference to a type descriptor for the specified type. */
  //StructBuilder & addTypeReference(const Type * type);

  /** Add a field which contains a reference to a type expression for the specified type. */
  //StructBuilder & addTypeReference(const TypeRef & type);

  /** Add a field containing a constant null pointer. */
  StructBuilder & addNullField(const Type * type);

  /** Add a field containing a constant null pointer. */
  StructBuilder & addNullField(const TypeRef & type);

  /** Add an integer field of the specified type. */
  StructBuilder & addIntegerField(const Type * type, int32_t value);

  /** Add an integer field with the same type as the variable 'var'. */
  StructBuilder & addIntegerField(VariableDefn * var, int32_t value);

  /** Add a String field. */
  StructBuilder & addStringField(const std::string & strval);

  /** Add a native array field consisting of elements of type 'elementType'. */
  StructBuilder & addArrayField(const Type * elementType, const ConstantList & values);

  /** Add a native array whose type is derived from the type of 'arrayVar' and whose
      elements consist of 'values. */
  StructBuilder & addArrayField(const VariableDefn * arrayVar, const ConstantList & values);

  /** Combine all of the members into a single structure field. */
  StructBuilder & combine();

  /** Build a constant struct from the member fields. */
  llvm::Constant * build() const;

  /** Build a constant struct from the member fields, and verify that it is the expected
      type. */
  llvm::Constant * build(const llvm::Type * expectedType) const;

private:
  ConstantList members_;
  CodeGenerator & gen_;
};

} // namespace tart

#endif // TART_GEN_STRUCTBUILDER_H
