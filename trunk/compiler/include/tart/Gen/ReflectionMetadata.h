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

  ReflectionMetadata(const Defn * reflectedDefn, ModuleMetadata & mmd)
    : reflectedDefn_(reflectedDefn)
    , mmd_(mmd)
    , var_(NULL)
    , methodTable_(NULL)
    , strm_(strmData_)
  {}

  ~ReflectionMetadata();

  void addTypeRef(const Type * type);
  void addASTDecl(const ASTDecl * ast);

  llvm::GlobalVariable * var() const { return var_; }
  void setVar(llvm::GlobalVariable * var) { var_ = var; }

  std::vector<llvm::Constant *> & methodTable() { return methodTable_; }
  const std::vector<llvm::Constant *> & methodTable() const { return methodTable_; }

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
  std::vector<llvm::Constant *> methodTable_;
  std::string strmData_;
  llvm::raw_string_ostream strm_;

  TypeArray derivedTypeRefs_;
  TypeArray compositeTypeRefs_;
  TypeArray enumTypeRefs_;
};

}

 // namespace tart

#endif // TART_META_METADATAWRITER_H
