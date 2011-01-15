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

/// -------------------------------------------------------------------
/// Class used to build the reflection metadata for a module or class.

class ReflectionMetadata {
public:
  typedef std::pair<const Type *, TagInfo> TypeArrayElement;
  typedef std::vector<TypeArrayElement> TypeArray;
  typedef llvm::DenseMap<const Type *, TagInfo, Type::CanonicalKeyInfo> TypeMap;

  typedef std::vector<llvm::Constant *> ConstantArray;

  ReflectionMetadata(const Defn * reflectedDefn)
    : reflectedDefn_(reflectedDefn)
    , strm_(strmData_)
    , methodBaseIndex_(0)
  {}

  ~ReflectionMetadata() {}

  void addTypeRef(const Type * type);
  size_t addGlobalRef(llvm::Constant * attribute);

  /** The method table contains only method pointers that are not already pointed to by
      the TypeInfoBlock for the class being reflected. */
  ConstantArray & methodRefs() { return methodRefs_; }
  const ConstantArray & methodRefs() const { return methodRefs_; }

  /** The table of pointers to global objects referred to by the type. */
  ConstantArray & globalRefs() { return globalRefs_; }
  const ConstantArray & globalRefs() const { return globalRefs_; }

  /** The base offset for indices into the method table. Method indices lower than this
      will use the dispatch table in the TypeInfoBlock; Method indices greater than or equal to
      this will use the method table. */
  size_t methodBaseIndex() const { return methodBaseIndex_; }
  void setMethodBaseIndex(size_t index) { methodBaseIndex_ = index; }

  // Sort all of the types by popularity and assign IDs.
  void assignIndices();

  void encodeTypeRef(const Type * type, llvm::raw_ostream & out);

  llvm::raw_string_ostream & strm() { return strm_; }
  std::string & strmData() { return strmData_; }

  const TypeArray & typeRefs() const { return typeRefs_; }

  void dump() const;

private:
  const Defn * reflectedDefn_;
  TypeMap types_;

  std::string strmData_;
  llvm::raw_string_ostream strm_;

  TypeArray typeRefs_;
  ConstantArray globalRefs_;
  ConstantArray methodRefs_;
  size_t methodBaseIndex_;
};

}

 // namespace tart

#endif // TART_META_METADATAWRITER_H
