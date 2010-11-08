/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_REFLECTIONMETADATA_H
#define TART_GEN_REFLECTIONMETADATA_H

#include "tart/Common/Agenda.h"
#include "tart/CFG/Type.h"

#include "tart/Meta/NameTable.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

namespace tart {

class Defn;
class Module;
class ASTDecl;

/// -------------------------------------------------------------------
/// List of reflected defns exported by a module.

class ModuleMetadata {
public:
  typedef llvm::DenseMap<const Type *, TagInfo, Type::CanonicalKeyInfo> TypeMap;

  ModuleMetadata(NameTable & names)
    : names_(names)
  {}

  NameTable & names() const { return names_; }
  const Agenda<const Defn> & defnsToExport() const { return defnsToExport_; }
  Agenda<const Defn> & defnsToExport() { return defnsToExport_; }
  const TypeMap & invokeMap() const { return invokeMap_; }
  TypeMap & invokeMap() { return invokeMap_; }

private:
  NameTable & names_;
  Agenda<const Defn> defnsToExport_;
  TypeMap invokeMap_;
};

/// -------------------------------------------------------------------
/// Class used to build the reflection metadata for a module or class.

class ReflectionMetadata {
public:
  typedef std::pair<const Type *, TagInfo> TypeArrayElement;
  typedef std::vector<TypeArrayElement> TypeArray;
  typedef llvm::DenseMap<const Type *, TagInfo, Type::CanonicalKeyInfo> TypeMap;

  typedef std::vector<llvm::Constant *> ConstantArray;

  ReflectionMetadata(const Defn * reflectedDefn, ModuleMetadata & mmd)
    : reflectedDefn_(reflectedDefn)
    , mmd_(mmd)
    , var_(NULL)
    , methodBaseIndex_(0)
    , methodTable_(NULL)
    , strm_(strmData_)
  {}

  ~ReflectionMetadata();

  void addTypeRef(const Type * type);
  void addASTDecl(const ASTDecl * ast);
  size_t addRetainedAttribute(llvm::Constant * attribute);

  llvm::GlobalVariable * var() const { return var_; }
  void setVar(llvm::GlobalVariable * var) { var_ = var; }

  /** The method table contains only method pointers that are not already pointed to by
      the TypeInfoBlock for the class being reflected. */
  ConstantArray & methodTable() { return methodTable_; }
  const ConstantArray & methodTable() const { return methodTable_; }

  /** The retained attribute table contains attribute instances that are needed by
      the class or members of the class. */
  ConstantArray & retainedAttrTable() { return retainedAttrs_; }
  const ConstantArray & retainedAttrTable() const { return retainedAttrs_; }

  /** The base offset for indices into the method table. Method indices lower than this
      will use the dispatch table in the TypeInfoBlock; Method indices greater than or equal to
      this will use the method table. */
  size_t methodBaseIndex() const { return methodBaseIndex_; }
  void setMethodBaseIndex(size_t index) { methodBaseIndex_ = index; }

  // Sort all of the types by popularity and assign IDs.
  void assignIndices();

  void encodeTypesTable(llvm::raw_ostream & out);
  void encodeTypeRef(const Type * type, llvm::raw_ostream & out);
  void encodeType(const Type * type, llvm::raw_ostream & out);

  llvm::raw_string_ostream & strm() { return strm_; }
  std::string & strmData() { return strmData_; }

  const TypeArray & derivedTypeRefs() const { return compositeTypeRefs_; }
  const TypeArray & compositeTypeRefs() const { return compositeTypeRefs_; }
  const TypeArray & enumTypeRefs() const { return enumTypeRefs_; }

  ModuleMetadata & mmd() { return mmd_; }

  void dump() const;

private:
  const Defn * reflectedDefn_;
  ModuleMetadata & mmd_;
  TypeMap types_;

  llvm::GlobalVariable * var_;
  size_t methodBaseIndex_;
  ConstantArray methodTable_;
  std::string strmData_;
  llvm::raw_string_ostream strm_;

  TypeArray derivedTypeRefs_;
  TypeArray compositeTypeRefs_;
  TypeArray enumTypeRefs_;

  ConstantArray retainedAttrs_;
};

}

 // namespace tart

#endif // TART_META_METADATAWRITER_H
